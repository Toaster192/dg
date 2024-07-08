#ifndef DG_SMG_POINTER_ANALYSIS_H_
#define DG_SMG_POINTER_ANALYSIS_H_
#include <set>
#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <iostream>
#include <fstream>
#include <filesystem>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Support/raw_ostream.h>

#include "dg/PointerAnalysis/Pointer.h"
#include "dg/PointerAnalysis/PointerAnalysis.h"

#include "dg/llvm/PointerAnalysis/LLVMPointerAnalysisOptions.h"
#include "dg/llvm/PointerAnalysis/LLVMPointsToSet.h"
#include "dg/llvm/PointerAnalysis/PointerAnalysis.h"
#include "dg/llvm/PointerAnalysis/PointerGraph.h"
#include "dg/llvm/PointerAnalysis/SMGObjects.h"

#include "dg/util/debug.h"

#include "json.hpp"

using json = nlohmann::json;

namespace std {
template<>
struct hash<dg::LLVMPointer>
{
    size_t operator()(const dg::LLVMPointer& ptr) const noexcept
    {
        size_t h1 = hash<llvm::Value*>{}(ptr.value);
        size_t h2 = hash<dg::Offset>{}(ptr.offset);
        return h1 ^ (h2 << 1);
    }
};
}

namespace dg {

using pta::Pointer;
using pta::PointerGraph;
using pta::LLVMPointerGraphBuilder;

typedef struct {bool null = false; bool nullWithOffset = false; bool unknown = false; bool invalidated = false;} SMGPTAFlags; // TODO provide the invalidated information
using CodeLoc = std::pair<int, int>;
using SMGPTASet = std::set<dg::LLVMPointer>;
using SMGPTAPair = std::pair<SMGPTASet, SMGPTAFlags>;
using SMGPTA = std::unordered_map<dg::LLVMPointer, SMGPTAPair>;
using GlobalSMGPTA = std::map<CodeLoc, SMGPTA>;
using PTA = std::unordered_map<const llvm::Value *, SMGPTAPair>;

inline bool operator==(const SMGPTAFlags& lhs, const SMGPTAFlags& rhs){
    return (lhs.null == rhs.null &&
            lhs.nullWithOffset == rhs.nullWithOffset &&
            lhs.unknown == rhs.unknown &&
            lhs.invalidated == rhs.invalidated);
}

inline bool operator!=(const SMGPTAFlags& lhs, const SMGPTAFlags& rhs){
    return !(lhs.null == rhs.null);
}

inline int count_flags(SMGPTAFlags F){
    return F.null + F.nullWithOffset + F.unknown + F.invalidated;
}

inline int merge_flags(SMGPTAFlags *target, const SMGPTAFlags source){
    int before_count = count_flags(*target);
    target->null |= source.null;
    target->nullWithOffset |= source.nullWithOffset;
    target->unknown |= source.unknown;
    target->invalidated |= source.invalidated;
    return count_flags(*target) - before_count;
}
inline int mergePTAPairs(SMGPTAPair &target, const SMGPTAPair source){
    int found = 0;
    for(auto pt : source.first){
        target.first.insert(pt);
        found++;
    }
    found += merge_flags(&target.second, source.second);
    return found;
}

inline void mergeSMGPTAs(SMGPTA &target, const SMGPTA source){
    for (auto PTPair : source){
        if (target.count(PTPair.first)){
            mergePTAPairs(target[PTPair.first], PTPair.second);
        } else {
            target[PTPair.first] = PTPair.second;
        }
    }
}

static bool ends_with(std::string_view str, std::string_view suffix)
{
    return str.size() >= suffix.size() && str.compare(str.size()-suffix.size(), suffix.size(), suffix) == 0;
}

static bool starts_with(std::string_view str, std::string_view prefix)
{
    return str.size() >= prefix.size() && str.compare(0, prefix.size(), prefix) == 0;
}

class SMG {

    void load(json &smg_json){
        //llvm::errs() << "Parsing SMG values" << "\n";
        for (auto v : smg_json["values"]){
            values.push_back(v.template get<SMGValue>());
        }
        //llvm::errs() << "Parsing SMG objects" << "\n";
        for (auto o : smg_json["objects"]){
            if (o["label"] == "field"){
                objects.push_back(o.template get<SMGVarObject>());
            } else {
                llvm::errs() << "Unknown object label: " << o["label"].template get<std::string>()  << "\n";
            }
        }
        //llvm::errs() << "Parsing SMG edges" << "\n";
        for (auto e : smg_json["edges"]){
            edges.push_back(e.template get<SMGEdge>());
        }
        //llvm::errs() << "Parsing SMG composite objects" << "\n";
        for (auto o : smg_json["compositeObjects"]){
            std::string label = o["label"].template get<std::string>();
            if (label == "region"){
                objects.push_back(o.template get<SMGRegionCompositeObject>());
            } else if (label == "DLS" || label == "SLS"){
                objects.push_back(o.template get<SMGLinkedListCompositeObject>());
            } else {
                llvm::errs() << "Unknown composite object label: " << label << "\n";
            }
        }

        return;
    }

  public:
    std::vector<SMGValue> values;
    std::vector<SMGObjectTypeVariant> objects;
    std::vector<SMGEdge> edges;

    SMG(json &smg_json){
        load(smg_json);
        //llvm::errs() << "Loaded " << values.size() << " values, " << objects.size() << " objects and " << edges.size() << " edges."  << "\n";
    }

    void dump(){
        for (auto obj : objects){
            llvm::errs() << "obj: " << objectVariantGetId(obj) << "\n";
        }
        for (auto val : values){
            llvm::errs() << "val: " << val.id << ", " << val.label << "\n";
        }
        for (auto edge : edges){
            llvm::errs() << "edge: "<<  edge.from << ", " << edge.to << ", " << edge.off << "\n";
        }
    }


    std::unordered_map<llvm::Value *, std::vector<SMGValue *>> getValueMap(){
        std::unordered_map<llvm::Value *, std::vector<SMGValue *>> valueMap;
        for (auto &val : values){
            if (val.is_var){
                valueMap[val.var.llvm_val].push_back(&val);
            } else if (val.is_mem){
                valueMap[val.llvm_val].push_back(&val);
            } else if (!val.is_null && !val.is_null_with_offset && !val.is_unknown){
                llvm::errs() << "Unknown value found in getValueMap(), id: " << val.id << " \n";
            }
        }
        return valueMap;
    }

    SMGPTA getPointsToSets(){
        SMGPTA pointsToSets;
        std::unordered_map<llvm::Value *, std::vector<SMGValue *>> valueMap = getValueMap();
        for (auto mapping : valueMap){
            bool has_edge = false;
            for (auto smgValue : mapping.second){
                for (auto val : values){
                    if (hasEdge(smgValue->id, val.id)){
                        has_edge = true;
                        SMGPTASet PTSet;
                        SMGPTAFlags PTFlags = {false};

                        if (val.is_var){
                            PTSet.insert(LLVMPointer(val.var.llvm_val, Offset(0)));
                        }
                        if (val.is_mem){
                            PTSet.insert(LLVMPointer(val.llvm_val, Offset(0)));
                        }
                        if (val.is_null){
                            PTFlags.null = true;
                        }
                        if (val.is_null_with_offset){
                            PTFlags.nullWithOffset = true;
                        }
                        if (val.is_unknown){
                            PTFlags.unknown = true;
                        }
                        /*
                        } else {
                            llvm::errs() << "Unknown value found in getPointsToSets(), id: " << val.id << " \n";
                        */

                        Offset offset(getEdgeOffset(smgValue->id, val.id));
                        if (pointsToSets.count(LLVMPointer(mapping.first, offset))){
                            mergePTAPairs(pointsToSets[LLVMPointer(mapping.first, offset)], std::make_pair(PTSet, PTFlags));
                        } else {
                            pointsToSets[LLVMPointer(mapping.first, offset)] = std::make_pair(PTSet, PTFlags);
                        }
                    /*
                    } else if (smgValue->is_mem){
                        SMGPTASet PTSet;
                        SMGPTAFlags PTFlags = {false};
                        Offset offset(0);
                        pointsToSets[LLVMPointer(mapping.first, offset)] = std::make_pair(PTSet, PTFlags);
                    */
                    }
                }
                if (!has_edge){
                    SMGPTASet PTSet;
                    SMGPTAFlags PTFlags = {false};
                    Offset offset(0);
                    pointsToSets[LLVMPointer(mapping.first, offset)] = std::make_pair(PTSet, PTFlags);
                }
            }
        }
        return pointsToSets;
    }

    void replaceGlobals(){
        std::vector<SMGVarObject*> globals;
        for (auto &obj : objects){
            if (std::holds_alternative<SMGVarObject>(obj)){
                SMGVarObject &var = std::get<SMGVarObject>(obj);
                if (var.llvm_val && llvm::isa<llvm::GlobalVariable>(var.llvm_val)){
                    globals.push_back(&var);
                }
            }
        }
        std::vector<int> to_remove;
        for (auto global : globals){
            // TODO: maybe fill out some other SMGValue attributes?
            SMGValue *val = new SMGValue();
            int var_id = global->id;
            val->id = var_id;
            val->var = *global;
            val->is_var = true;
            val->llvm_val = global->llvm_val;
            values.push_back(*val);
            to_remove.push_back(var_id);
        }
        // Needs to be done in a separate loop otherwise the pointers get shifted
        for (int var_id : to_remove){
            objects.erase(std::remove_if(objects.begin(), objects.end(), [&var_id](SMGObjectTypeVariant o){return objectVariantGetId(o) == var_id;}), objects.end());
        }
        return;
    }
    
    SMGValue* getFirstValueByObjID(int obj_id){
        // look for offset == 0 first
        for (auto &value : values){
            if (value.target_spec_label == "TS_FIRST" && value.obj == obj_id && value.offset_low == 0){
                 return &value;
            }
        }
        for (auto &value : values){
            if (value.target_spec_label == "TS_FIRST" && value.obj == obj_id){
                 return &value;
            }
        }
        return nullptr;
    }

    void replaceTS_ALL(){
        auto it = values.begin();
        while (it != values.end()){
            auto val = *it;
            if (val.target_spec_label != "TS_ALL"){
                ++it;
                continue;
            }
            SMGValue* first_value = getFirstValueByObjID(val.obj);
            if (first_value == nullptr){
                llvm::errs() << "TS_ALL ID " << val.id << " no TS_FIRST found to point to." << "\n";
            }
            auto connected_edges = getEdgesWithId(val.id, edges);
            auto to_edges = connected_edges.first;
            auto from_edges = connected_edges.second;
            if (to_edges.size() != 1 || from_edges.size() != 1){
                llvm::errs() << "TS_ALL ID " << val.id << " unexpected amount of edges found" << "\n";
            }

            // Remove the TS_ALL -> list edge and point whichever edge was pointing at TS_ALL to the TS_FIRST of the list
            int obj_id = val.obj;
            int val_id = val.id;
            edges.erase(std::remove_if(edges.begin(), edges.end(), [&obj_id,&val_id](SMGEdge e){return e.to == obj_id && e.from == val_id;}), edges.end());
            replaceEdgeTargets(val_id, first_value->id);
            // Nothing should be pointing to TS_ALL anymore so we can remove it
            values.erase(it);
        }
        return;
    }

    void mapLLVMValues(llvm::Module *m){
        std::vector<SMGVarObject*> vars;
        for (auto &obj : objects){
            if (std::holds_alternative<SMGVarObject>(obj)){
                SMGVarObject &var = std::get<SMGVarObject>(obj);
                if (var.var_name != ""){
                    vars.push_back(&var);
                }
            }
        }

        for (auto &obj : objects){
            if (std::holds_alternative<SMGRegionCompositeObject>(obj)){
                SMGRegionCompositeObject &region = std::get<SMGRegionCompositeObject>(obj);
                for (auto &region_obj : region.objects){
                    if (std::holds_alternative<SMGVarObject>(region_obj)){
                        SMGVarObject &var = std::get<SMGVarObject>(region_obj);
                        if (var.var_name != ""){
                            vars.push_back(&var);
                        }
                    }
                }
            }
        }
        std::vector<SMGValue*> mems;
        for (auto &val : values){
            if (val.is_mem){
                mems.push_back(&val);
            }
        }
        for (auto &F : *m){
            /*
            if (func_name != "" && F.getName() != func_name){
                continue;
            }
            */
            for(llvm::inst_iterator I = llvm::inst_begin(F), E = llvm::inst_end(F); I != E; ++I){
                /*
                llvm::errs() << "inst: " << *I  << "\n";
                if (I->getNumOperands()){
                    llvm::errs() << "operand 0: " << *I->getOperand(0) << "\n";
                }
                */
                llvm::DebugLoc dbg = I->getDebugLoc();
                if (!dbg){
                    //llvm::errs() << "No debug info found!\n";
                    continue;
                }
                for (auto &var : vars){
                    if (dbg.getLine() == (unsigned) var->var_loc_line && dbg.getCol() == (unsigned) var->var_loc_column){
                        if(!llvm::isa<llvm::MetadataAsValue>(I->getOperand(0))){
                            // Shouldn't happen
                            continue;
                        }
                        llvm::Metadata *Meta = llvm::cast<llvm::MetadataAsValue>(I->getOperand(0))->getMetadata();
                        llvm::Value *V = llvm::cast <llvm::ValueAsMetadata>(Meta)->getValue();
                        var->llvm_val = V;
                        //llvm::errs() << "val: " << *var->llvmVal  << "\n";
                        auto it = std::find(vars.begin(), vars.end(), var);
                        vars.erase(it);
                        // Can't break as there may be two objects of the same variable for whatever reason
                    }
                }
                auto it = mems.begin();
                while (it != mems.end()){
                    SMGValue* val = *it;
                    if (dbg.getLine() == (unsigned) val->mem_loc_line && dbg.getCol() == (unsigned) val->mem_loc_column){
                        llvm::Value *V = llvm::dyn_cast<llvm::Value>(&(*I));
                        val->llvm_val = V;
                        //llvm::errs() << "val: " << *val->llvmVal  << "\n";
                        mems.erase(it);
                    } else {
                        ++it;
                    }
                }
            }
        }
        // Map global variables
        std::vector<SMGVarObject*>::iterator it = vars.begin();
        while (it != vars.end()){
            auto var = *it;

            if (auto globVar = m->getGlobalVariable(var->var_name)){
                llvm::Value *V = llvm::cast <llvm::Value>(globVar);
                var->llvm_val = V;
                vars.erase(it);
            } else if (auto namedVar = m->getNamedGlobal(var->var_name)){
                llvm::Value *V = llvm::cast <llvm::Value>(namedVar);
                var->llvm_val = V;
                vars.erase(it);
            } else {
                ++it;
            }
        }
        std::vector<SMGValue*> strings;
        for (auto &val : values){
            if (val.label == "str"){
                strings.push_back(&val);
            }
        }
        for(auto G = m->global_begin(); G != m->global_end(); ++G){
            if (!G->hasInitializer()){
                continue;
            }
            auto constant = llvm::dyn_cast<llvm::ConstantDataArray>(G->getInitializer());
            if (!constant || !constant->isCString()){
                continue;
            }
            auto it = strings.begin();
            while (it != strings.end()){
                SMGValue* val = *it;

                if (constant->getAsCString() == val->value){
                    llvm::Value *V = llvm::dyn_cast<llvm::Value>(&(*G));
                    val->llvm_val = V;
                    val->is_mem = true;
                    strings.erase(it);
                } else {
                    ++it;
                }
            }
        }
        std::vector<SMGValue*> funcs;
        for (auto &val : values){
            if (val.label == "fnc"){
                funcs.push_back(&val);
            }
        }
        for (auto &F : *m){
            auto name = F.getName();
            auto it = funcs.begin();
            while (it != funcs.end()){
                SMGValue* val = *it;

                if (name == val->value){
                    llvm::Value *V = llvm::dyn_cast<llvm::Value>(&F);
                    val->llvm_val = V;
                    val->is_mem = true;
                    funcs.erase(it);
                } else {
                    ++it;
                }
            }
        }
        if (vars.size()){
            llvm::errs() << "Not every SMG variable mapped!" << "\n";
            for (auto var : vars){
                llvm::errs() << "Var: " << var->id << "\n";
            }
        }
        if (mems.size()){
            llvm::errs() << "Not every SMG heap object mapped!" << "\n";
            for (auto mem : mems){
                llvm::errs() << "Mem: " << mem->id << "\n";
            }
        }
        if (strings.size()){
            llvm::errs() << "Not every SMG string object mapped!" << "\n";
            for (auto str : strings){
                llvm::errs() << "Str: " << str->id << "\n";
                llvm::errs() << "Val: " << str->value << "\n";
            }
        }
        if (funcs.size()){
            llvm::errs() << "Not every SMG function object mapped!" << "\n";
            for (auto str : funcs){
                llvm::errs() << "Fnc: " << str->id << "\n";
                llvm::errs() << "value: " << str->value << "\n";
            }
        }
    }

    void replaceEdgeTargets(int from, int to){
        for (auto &edge : edges){
            if (edge.from == from){
                edge.from = to;
            }
            if (edge.to == from){
                edge.to = to;
            }
        }
    }

    bool hasEdge(int from, int to){
        for (auto edge : edges){
            if (edge.from == from && edge.to == to){
                return true;
            }
        }
        return false;
    }

    int getEdgeOffset(int from, int to){
        for (auto edge : edges){
            if (edge.from == from && edge.to == to){
                return edge.off;
            }
        }
        llvm::errs() << "Calling getEdgeOffset on non-existent edge (from: " << from << ", to: " << to << ")\n";
        return 0;
    }

    SMGEdge* getEdge(int from, int to){
        for (auto &edge : edges){
            if (edge.from == from && edge.to == to){
                return &edge;
            }
        }
        llvm::errs() << "Calling getEdge on non-existent edge" << "\n";
        return nullptr;
    }

    SMGValue* getObjectsPtrValue(int objectId){
        for (auto edge : edges){
            if (edge.from == objectId){
                return &*getValueById(edge.to, values);
            }
        }
        return nullptr;
    }
    
    void removeEdgesOfObjects(std::vector<SMGSimpleObjectTypeVariant> objects){
        for (auto obj : objects){
            int id = objectVariantGetId(obj);
            edges.erase(std::remove_if(edges.begin(), edges.end(), [&id](SMGEdge e){return e.to == id || e.from == id;}), edges.end());
        }
    }

    void removeAnonymousObjects(){
        //llvm::errs() << "Removing anonymous objects, total objects: " << objects.size() << "\n";
        auto it = objects.begin();
        while (it != objects.end()){
            auto obj = *it;
            if (std::holds_alternative<SMGVarObject>(obj)){
                SMGVarObject var = std::get<SMGVarObject>(obj);
                if (var.var_name == ""){
                    int var_id = var.id;

                    edges.erase(std::remove_if(edges.begin(), edges.end(), [&var_id](SMGEdge e){return e.from == var_id || e.to == var_id;}), edges.end());

                    objects.erase(it);
                    continue;
                }
            }
            ++it;
        }
        //llvm::errs() << "Done removing anonymous objects, total objects: " << objects.size() << "\n";
    }

    void copyCompositeObjectObjects(SMGLinkedListCompositeObject &linked_list, SMGLinkedListCompositeObject old){
        for(auto obj : old.objects){
            int new_obj_id = findUniqueId(values, objects, edges);
            if (std::holds_alternative<SMGRegionObject>(obj)){
                SMGRegionObject region = std::get<SMGRegionObject>(obj);
                linked_list.objects.push_back(SMGRegionObject(region, new_obj_id));
                linked_list.obj_id = new_obj_id;
                break;
            }
        }
        for(auto obj : old.objects){
            int new_obj_id = findUniqueId(values, objects, edges);
            if (std::holds_alternative<SMGEmptyObject>(obj)){
                SMGEmptyObject empty = std::get<SMGEmptyObject>(obj);
                linked_list.objects.push_back(SMGEmptyObject(empty, new_obj_id));
            } else if (std::holds_alternative<SMGRegionObject>(obj)){
                continue;
            } else {
                llvm::errs() << "Unknown object type in copyCompositeObjectObjects" << "\n";
            }
            int old_obj_id = old.obj_id;
            int target = objectVariantGetId(obj);
            SMGEdge* edge = getEdge(old_obj_id, target);
            edges.push_back(SMGEdge(linked_list.obj_id, new_obj_id, edge->label, edge->off));
        }
    }

    void expandCompositeObject(SMGRegionCompositeObject region, std::vector<SMGObjectTypeVariant> &newObjects){
        for (auto &o : region.objects){
            if (auto converted = convertSMGObjectVariant(o)){
                newObjects.push_back(*converted);
            } else {
                llvm::errs() << "Failed to convert to object variant in composite object " << region.id << "\n";
            }
        }
    }

    void expandCompositeObject(SMGLinkedListCompositeObject linked_list, std::vector<SMGObjectTypeVariant> &newObjects){
        for (auto &o : linked_list.objects){
            if (auto converted = convertSMGObjectVariant(o)){
                newObjects.push_back(*converted);
            } else {
                llvm::errs() << "Failed to convert to object variant in composite object " << linked_list.id << "\n";
            }
        }
    }

    void chainListSegments(SMGLinkedListCompositeObject first, SMGLinkedListCompositeObject second){
        // connect the two segments via next and prev pointers
        int first_next = first.getNextObjectId(edges);
        edges.push_back(SMGEdge(first_next, second.obj_id, 0));

        int first_prev = -1;
        int second_prev = -1;
        if(first.label == "DLS"){
            first_prev = first.getPrevObjectId(edges);

            second_prev = second.getPrevObjectId(edges);
            edges.push_back(SMGEdge(second_prev, first.obj_id, 0));
        }
        // copy the data pointers of the first segment to the second
        for (auto o : first.objects){
            int o_id = objectVariantGetId(o);
            if (o_id == first_next){
                continue;
            }
            if (o_id == first_prev){
                continue;
            }
            if (o_id == first.obj_id){
                continue;
            }

            int obj_offset = getEdgeOffset(first.obj_id, o_id);
            int second_object = second.getObjectIdByOffset(edges, obj_offset);
            
            for (SMGEdge edge : edges){
                if (edge.from == o_id){
                    edges.push_back(SMGEdge(second_object, edge.to, edge.off));
                }
            }
        }
    }

    void simplifyCompositeObjects(){
        //llvm::errs() << "Simplifying composite objects, total objects: " << objects.size() << "\n";
        std::vector<SMGObjectTypeVariant> newObjects;
        auto it = objects.begin();
        while (it != objects.end()){
            auto obj = *it;
            if (std::holds_alternative<SMGRegionCompositeObject>(obj)){
                SMGRegionCompositeObject region = std::get<SMGRegionCompositeObject>(obj);
                expandCompositeObject(region, newObjects);
                objects.erase(it);
            } else if (std::holds_alternative<SMGLinkedListCompositeObject>(obj)){
                SMGLinkedListCompositeObject linked_list = std::get<SMGLinkedListCompositeObject>(obj);
                //SMGValue* first_value = linked_list.getFirstValue(values);
                SMGValue* last_value = linked_list.getLastValue(values);

                int next_obj_id = linked_list.getNextObjectId(edges);
                SMGEmptyObject* next_obj = &std::get<SMGEmptyObject>(*getObjectById(next_obj_id, linked_list.objects));
                SMGValue* next_value = getObjectsPtrValue(next_obj->id);

                int prev_obj_id = 0;
                SMGEmptyObject* prev_obj = nullptr;
                SMGValue* prev_value = nullptr;
                if(linked_list.label == "DLS"){
                    prev_obj_id = linked_list.getNextObjectId(edges);
                    prev_obj = &std::get<SMGEmptyObject>(*getObjectById(prev_obj_id, linked_list.objects));
                    prev_value = getObjectsPtrValue(prev_obj->id);
                }

                /*
                if (linked_list.seg_min_len == 0){
                    auto obj_it = getObjectById(linked_list.obj_id, linked_list.objects);
                    SMGRegionObject region = std::get<SMGRegionObject>(*obj_it);
                    // Set both first and last to a virtual memory location
                    if (last_value){
                        first_value->memory_location = last_value->memory_location = region.label;
                        first_value->size_low        = last_value->size_low        = region.size_low;
                        first_value->size_high       = last_value->size_high       = region.size_high;
                        first_value->is_unknown      = last_value->is_unknown      = true;
                    } else {
                        first_value->memory_location = region.label;
                        first_value->size_low        = region.size_low;
                        first_value->size_high       = region.size_high;
                        first_value->is_unknown      = true;
                    }

                    // Add edge to the next / prev pointers
                    if(linked_list.label == "DLS"){
                        edges.push_back(SMGEdge(first_value->id, prev_value->id, getEdgeOffset(first_value->id, region.id)));
                    }
                    if(last_value){
                        edges.push_back(SMGEdge(last_value->id, next_value->id, getEdgeOffset(last_value->id, region.id)));
                    }
                    // Add a bypass of the first / last values
                    std::vector<SMGEdge> newEdges;
                    for (SMGEdge edge : edges){
                        if (linked_list.label == "DLS" && edge.to == first_value->id){
                            newEdges.push_back(SMGEdge(edge.from, prev_value->id, edge.off));
                        }
                        if (last_value && edge.to == last_value->id){
                            newEdges.push_back(SMGEdge(edge.from, next_value->id, edge.off));
                        }
                    }
                    for (SMGEdge edge : newEdges){
                        edges.push_back(edge);
                    }
                    // Remove all edges inside the linked list before the objects are abandoned
                    removeEdgesOfObjects(linked_list.objects);
                */
                /*
                } else if (linked_list.seg_min_len == 1){
                    auto obj_it = getObjectById(linked_list.obj_id, linked_list.objects);
                    SMGRegionObject region = std::get<SMGRegionObject>(*obj_it);
                    // Set the last virtual object to a virtual memory location
                    last_value->memory_location = region.label;
                    last_value->size_low        = region.size_low;
                    last_value->size_high       = region.size_high;
                    last_value->is_unknown      = true;
                    // Disconnect the last object and the linked list
                    int region_id = region.id;
                    int last_value_id = last_value->id;
                    edges.erase(std::remove_if(edges.begin(), edges.end(), [&last_value_id, &region_id](SMGEdge e){return e.from == last_value_id && e.to == region_id;}), edges.end());
                    // Add a pointer from first->next to last (unknown)
                    edges.push_back(SMGEdge(next_obj->id, last_value->id, 0));

                    expandCompositeObject(linked_list, newObjects);
                } else if (linked_list.seg_min_len == 1 || linked_list.seg_min_len == 2){
                */
                // copy the linked list segment
                SMGLinkedListCompositeObject copy = SMGLinkedListCompositeObject(linked_list, findUniqueId(values, objects, edges));
                copyCompositeObjectObjects(copy, linked_list);

                // set the last value .obj attribute to the new segment region
                if(last_value){
                    last_value->obj = copy.obj_id;
                }

                /*
                // create an unknown value to point to between the two border segments etc.
                SMGValue unknown(findUniqueId(values, objects, edges), "unknown");
                unknown.is_unknown = true;
                values.push_back(unknown);

                // first->next = unknown, last->prev = uknown
                edges.push_back(SMGEdge(next_obj_id, unknown.id, 0));
                int copy_prev_id = copy.getPrevObjectId(edges);
                edges.push_back(SMGEdge(copy_prev_id, unknown.id, 0));
                */
                chainListSegments(linked_list, copy);

                // last->next = next_value
                int copy_next_id = copy.getNextObjectId(edges);
                edges.push_back(SMGEdge(copy_next_id, next_value->id, 0));

                // connect last with the copied segment
                if (last_value){
                    edges.push_back(SMGEdge(last_value->id, copy.obj_id, 0));
                }
                // remove outdated connections
                int first_obj = linked_list.obj_id;
                int next_value_id = next_value->id;
                if (last_value){
                    int last_value_id = last_value->id;
                    edges.erase(std::remove_if(edges.begin(), edges.end(), [&last_value_id, &first_obj](SMGEdge e){return e.from == last_value_id && e.to == first_obj;}), edges.end());
                }
                edges.erase(std::remove_if(edges.begin(), edges.end(), [&next_obj_id, &next_value_id](SMGEdge e){return e.from == next_obj_id && e.to == next_value_id;}), edges.end());

                expandCompositeObject(copy, newObjects);
                expandCompositeObject(linked_list, newObjects);
                /*
                } else {
                    llvm::errs() << "Unknown seg_min_len in a linked list composite object id: " << linked_list.id << "\n";
                }
                */
                objects.erase(it);
            } else {
                ++it;
            }
        }
        objects.insert(objects.end(), newObjects.begin(), newObjects.end());
        //llvm::errs() << "Done simplifying composite objects, new total objects: " << objects.size() << "\n";
        //dump();
    }

    int findOriginalId(SMGValue* dupe){
        for (auto val : values){
            if (val.id != dupe->id && val.obj == dupe->obj && val.offset_low == 0){
                return val.id;
            }
        }
        return 0;
    }

    void simplifyValues(){
        std::vector<SMGVarObject> varObjects;
        for (auto &obj : objects){
            if (std::holds_alternative<SMGVarObject>(obj)){
                varObjects.push_back(std::get<SMGVarObject>(obj));
            }
        }
        //llvm::errs() << "Removing non-pointer values (current total: " << values.size() << ")\n";
        auto val_it = values.begin();
        while (val_it != values.end()){
            auto &val = *val_it;
            int val_id = val.id;
            if (val.label == "int"){
                val.is_null_with_offset = true;
            }
            if (val.label == "int_range" || val.label == "real"){ // let "fnc", "str" and "int" pass
                values.erase(val_it);
                edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id;}), edges.end());
            } else {
                ++val_it;
            }
        }

        //llvm::errs() << "Merging values (" << values.size() << ") with variable and region objects (total objects: " << objects.size() << ")\n";
        // merge values with variables
        for (auto var : varObjects){
            if (not var.llvm_val){
                continue;
            }
            for (auto &val : values){
                if(hasEdge(var.id, val.id)){
                    if (val.is_var || val.is_null || val.is_unknown || val.is_null_with_offset){
                        continue;
                    }
                    int obj_id = val.obj;
                    int val_id = val.id;
                    int var_id = var.id;

                    auto it = getObjectById(obj_id, objects);
                    if (it == objects.end()){
                        llvm::errs() << "ID " << obj_id << " not found when merging variables!" << "\n";
                        continue;
                    }
                    if (!std::holds_alternative<SMGRegionObject>(*it)){
                        llvm::errs() << "ID " << obj_id << " expected region object!" << "\n";
                        continue;
                    }
                    SMGRegionObject obj = std::get<SMGRegionObject>(*it);

                    val.memory_location = obj.label;
                    val.size_low = obj.size_low;
                    val.size_high = obj.size_high;

                    replaceEdgeTargets(obj_id, val_id);

                    replaceEdgeTargets(var_id, val_id);
                    val.is_var = true;
                    val.var = var;

                    edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id && e.from == val_id;}), edges.end());
                    objects.erase(it);

                    objects.erase(std::remove_if(objects.begin(), objects.end(), [&var_id](SMGObjectTypeVariant o){return objectVariantGetId(o) == var_id;}), objects.end());
                    break;
                }
            }
        }
        //llvm::errs() << "Merging values (" << values.size() << ") with region objects (total objects: " << objects.size() << ")\n";
        std::vector<SMGValue*> duplicateValues;
        // merge values with memory locations
        for (auto &val : values){
            if (val.is_var || val.is_null || val.is_null_with_offset || val.is_unknown){
                continue;
            }
            if (val.lonely){
                //llvm::errs() << "Found lonely value: " << val.id << ", label: " << val.label << "\n";
                continue;
            }
            if (val.offset_low){
                duplicateValues.push_back(&val);
                continue;
            }
            int obj_id = val.obj;
            int val_id = val.id;
            auto it = getObjectById(obj_id, objects);
            if (it == objects.end()){
                llvm::errs() << "ID " << obj_id << " not found when merging regions!" << "\n";
                continue;
            }
            if (!std::holds_alternative<SMGRegionObject>(*it)){
                continue;
            }
            SMGRegionObject obj = std::get<SMGRegionObject>(*it);

            val.memory_location = obj.label;
            val.size_low = obj.size_low;
            val.size_high = obj.size_high;

            objects.erase(it);
            edges.erase(std::remove_if(edges.begin(), edges.end(), [&obj_id,&val_id](SMGEdge e){return e.to == obj_id && e.from == val_id;}), edges.end());
            replaceEdgeTargets(obj_id, val_id);
        }
        //llvm::errs() << "Merging values (" << values.size() << ") with static variable objects (total objects: " << objects.size() << ")\n";
        for (auto var : varObjects){
            if (not var.llvm_val){
                continue;
            }
            for (auto &val : values){
                if(val.obj == var.id){
                    if (val.is_var || val.is_null || val.is_unknown || val.is_null_with_offset){
                        continue;
                    }
                    int val_id = val.id;
                    int var_id = var.id;

                    val.memory_location = var.label;

                    replaceEdgeTargets(var_id, val_id);
                    val.is_var = true;
                    val.var = var;

                    edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id && e.from == val_id;}), edges.end());

                    objects.erase(std::remove_if(objects.begin(), objects.end(), [&var_id](SMGObjectTypeVariant o){return objectVariantGetId(o) == var_id;}), objects.end());
                    break;
                }
            }
        }
        if (duplicateValues.size()){
            //llvm::errs() << "Merging duplicate values (" << values.size() << ") objects (" << objects.size() << ")\n";
            for (auto val : duplicateValues){
                int val_id = val->id;
                int original_id = findOriginalId(val);

                for (auto &edge : edges){
                    if (edge.to == val_id){
                        edge.to = original_id;
                        edge.off += val->offset_low;
                    }
                }

                edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.from == val_id;}), edges.end());
            }
        }
        //llvm::errs() << "Done merging values (" << values.size() << ") objects (" << objects.size() << ")\n";
    }

    void simplifyEmptyObjects(){
        //llvm::errs() << "Finding empty objects in " << objects.size() << " objects." << "\n";
        std::vector<SMGEmptyObject> emptyObjects;
        for (auto &obj : objects){
            if (std::holds_alternative<SMGEmptyObject>(obj)){
                emptyObjects.push_back(std::get<SMGEmptyObject>(obj));
            } else if (std::holds_alternative<SMGRegionObject>(obj)){
                continue;
            } else if (std::holds_alternative<SMGGenericObject>(obj)){
                continue;
            } else if (std::holds_alternative<SMGVarObject>(obj)){
                continue;
            } else {
                llvm::errs() << "Unknown object type found in simplifyEmptyObjects()" << "\n";
            }
        }

        //llvm::errs() << "Found " << emptyObjects.size() << " empty objects." << "\n";
        for (SMGEmptyObject &empty : emptyObjects){
            auto connected_edges = getEdgesWithId(empty.id, edges);
            auto to_edges = connected_edges.first;
            auto from_edges = connected_edges.second;

            if (to_edges.size() == 0){
                llvm::errs() << "Found empty object id " << empty.id << " with no \"to\" edge!\n";
                for (auto from_edge : from_edges){
                    edges.erase(std::find(edges.begin(), edges.end(), *from_edge));
                }
                objects.erase(std::remove_if(objects.begin(), objects.end(), [&empty](SMGObjectTypeVariant o){return objectVariantGetId(o) == empty.id;}), objects.end());
                continue;
            }

            if (to_edges.size() != 1){
                llvm::errs() << "More than 1 edge(" << to_edges.size() << ") pointing at object id " << empty.id << "!\n";
                continue;
            }
            SMGEdge to_edge = *to_edges[0];
            for (auto from_edge : from_edges){
                from_edge->from = to_edge.from;
                from_edge->off += to_edge.off;
            }

            edges.erase(std::find(edges.begin(), edges.end(), to_edge));
            objects.erase(std::remove_if(objects.begin(), objects.end(), [&empty](SMGObjectTypeVariant o){return objectVariantGetId(o) == empty.id;}), objects.end());
        }
        //llvm::errs() << "Done removing empty objects at " << objects.size() << ".\n";
    }

    void removeUnmergedValues(){
        //llvm::errs() << "Removing unmerged values, total values: " << values.size() << "\n";
        auto it = values.begin();
        while (it != values.end()){
            auto val = *it;
            int val_id = val.id;
            if (val.is_mem || val.is_var || val.is_null || val.is_null_with_offset || val.is_unknown){
                ++it;
            } else {
                //llvm::errs() << "Removing unmerged value: " << val_id << "\n";
                edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id || e.from == val_id;}), edges.end());
                values.erase(it);
            }
        }
        //llvm::errs() << "Done removing unmerged values, total values: " << values.size() << "\n";
    }

    void removeStrandedValues(){
        //llvm::errs() << "Removing stranded values, total values: " << values.size() << "\n";
        auto it = values.begin();
        while (it != values.end()){
            auto val = *it;
            auto connected_edges = getEdgesWithId(val.id, edges);
            auto to_edges = connected_edges.first;
            auto from_edges = connected_edges.second;
            if ((to_edges.size() + from_edges.size()) == 0){
                values.erase(it);
            } else {
                ++it;
            }
        }
        //llvm::errs() << "Done removing stranded values, total values: " << values.size() << "\n";
    }

    void simplify(){
        replaceGlobals();
        replaceTS_ALL();
        simplifyCompositeObjects();
        simplifyValues();
        simplifyEmptyObjects();
        removeUnmergedValues();
        removeAnonymousObjects();
        //removeStrandedValues();
        //llvm::errs() << "Simplified to " << values.size() << " values, "
        //             << objects.size() << " objects, "
        //             << edges.size() << " edges.\n";
    }
};

class SMGLLVMPointsToSet : public LLVMPointsToSetImplTemplate<SMGPTASet> {
    SMGPTAFlags flags{false, false, false, false};
    int flag_count;
    /*
    void _findNextReal() override {
        while (it != PTSet.end()) {
            if (*it.first.empty()){
                break;
            } else {
                ++it;
            }
        }
    }
    */
    void _findNextReal() override {} // TODO maybe not needed?

    void set_flags(SMGPTAFlags F){
        flags.null = F.null;
        flags.nullWithOffset = F.nullWithOffset;
        flags.unknown = F.unknown;
        flags.invalidated = F.invalidated;
        flag_count = count_flags(F);
    }

  public:
    SMGLLVMPointsToSet(SMGPTASet &S, SMGPTAFlags F)
            : LLVMPointsToSetImplTemplate(std::move(S)) {
        initialize_iterator();
        set_flags(F);
    }

    /*
    size_t size() const override { return PTSet.count(); }

    LLVMPointer getKnownSingleton() const override {
        assert(isKnownSingleton());
        return {_getValue(*PTSet.begin()), Offset::UNKNOWN};
    }
    */

    // TODO these methods need proper implementation
    bool hasUnknown() const override {
        return flags.unknown;
    }

    bool hasNull() const override { 
        return flags.null;
    }

    bool hasNullWithOffset() const override {
        return flags.nullWithOffset;
    }

    bool hasInvalidated() const override {
        return flags.invalidated;
    }
    size_t size() const override {
        return PTSet.size() + flag_count; //Not sure if this is supposed to include the flags but eh
    }

    LLVMPointer getKnownSingleton() const override {
        assert(isKnownSingleton());
        return *PTSet.begin();
    }

    LLVMPointer get() const override {
        assert(it != PTSet.end() && "Dereferenced end() iterator");
        return *it;
    }
};
///
// Integration of pointer analysis from predator SMG
class SMGPointerAnalysis : public LLVMPointerAnalysis {
    llvm::Module *_module{nullptr};
    const char *_smg_json_dir_path{nullptr};
    PTA _pts;

    LLVMPointsToSet mapSMGPointsTo(SMGPTAPair S) {
        auto *pts = new SMGLLVMPointsToSet(S.first, S.second);
        return pts->toLLVMPointsToSet();
    }

  public:
    SMGPointerAnalysis(llvm::Module *M,
                       const LLVMPointerAnalysisOptions &opts)
            : LLVMPointerAnalysis(opts), _module(M), _smg_json_dir_path(opts.smg_json_dir_path.c_str()) {}

    ~SMGPointerAnalysis() override {}

    bool hasPointsTo(const llvm::Value *val) override {
        return _pts.count(val);
    }

    ///
    // Get the points-to information for the given LLVM value.
    // The return object has methods begin(), end() that can be used
    // for iteration over (llvm::Value *, Offset) pairs of the
    // points-to set. Moreover, the object has methods hasUnknown()
    // and hasNull() that reflect whether the points-to set of the
    // LLVM value contains unknown element of null.
    LLVMPointsToSet getLLVMPointsTo(const llvm::Value *val) override {
        if (!hasPointsTo(val)){
            return mapSMGPointsTo({SMGPTASet(), {false, false, /*unknown=*/ true, false}});
        }
        return mapSMGPointsTo(_pts[val]);
    }

    ///
    // This method is the same as getLLVMPointsTo, but it returns
    // also the information whether the node of pointer analysis exists
    // (opposed to the getLLVMPointsTo, which returns a set with
    // unknown element when the node does not exists)
    std::pair<bool, LLVMPointsToSet>
    getLLVMPointsToChecked(const llvm::Value *val) override {
        return {hasPointsTo(val), getLLVMPointsTo(val)};
    }

    void dumpSMGPointsTo(SMG smg){
        SMGPTA pointsToSets = smg.getPointsToSets();
        dumpSMGPTAPointsTo(pointsToSets);
    }

    void dumpSMGPTAPointsTo(SMGPTA pointsToSets){
        for (auto PTPair : pointsToSets){
            llvm::errs() << "SMG points to info: " << *PTPair.first.value << " + " << *PTPair.first.offset << "\n";
            SMGPTASet& values = PTPair.second.first;
            SMGPTAFlags& flags = PTPair.second.second;
            for (auto ptPair : values){
                llvm::errs() << "                 ->" << *ptPair.value << " + " << *ptPair.offset << "\n";
            }
            if (flags.null){
                llvm::errs() << "                 ->  null" << "\n";
            }
            if (flags.nullWithOffset){
                llvm::errs() << "                 ->  null + ?" << "\n";
            }
            if (flags.unknown){
                llvm::errs() << "                 ->  unknown" << "\n";
            }
            if (flags.invalidated){
                llvm::errs() << "                 ->  invalidated" << "\n";
            }
        }
    }

    void dumpGlobalSMGPTAPointsTo(GlobalSMGPTA global_pta){
        for (auto codelocPTAPair : global_pta){
            /**/
            CodeLoc loc = codelocPTAPair.first;
            llvm::errs() << "For line: " << loc.first << " and column: " << loc.second << "\n";
            /**/
            dumpSMGPTAPointsTo(codelocPTAPair.second);
        }
    }


    /*
    std::vector<int> findOffsets(const SMGPTA pta, llvm::Value *value){
        std::vector<int> offsets;
        for (auto PTPair : pta){
            if (PTPair.first.value == value){
                offsets.push_back(*PTPair.first.offset);
            }
        }
        return offsets;
    }
    */

    PTA calculateAssistedPTA(GlobalSMGPTA global_smg_pts){
        PTA pts;
        PTA returns;
        for (auto &F : *_module){
            SMGPTA local_pts;
            for (llvm::inst_iterator I = llvm::inst_begin(F), E = llvm::inst_end(F); I != E; ++I){
                llvm::errs() << "inst: " << *I  << "\n";
                llvm::DebugLoc dbg = I->getDebugLoc();
                if (dbg){
                    CodeLoc loc = {dbg.getLine(), dbg.getCol()};
                    auto local_pts_it = global_smg_pts.upper_bound(loc);
                    if (local_pts_it != global_smg_pts.begin()){
                        --local_pts_it;
                    }
                    local_pts = local_pts_it->second;
                    //llvm::errs() << "line: " << dbg.getLine() << ", col: " << dbg.getCol() << "\n";
                    dumpSMGPTAPointsTo(local_pts);
                }
                llvm::Value *V = llvm::dyn_cast<llvm::Value>(&(*I));
                if (llvm::isa <llvm::LoadInst> (*I)){
                    llvm::Value *op = I->getOperand(0);
                    LLVMPointer target(op, Offset(0));

                    if (local_pts.count(target) && (local_pts[target].first.size() != 0 || count_flags(local_pts[target].second))){
                        mergePTAPairs(pts[V], local_pts[target]);
                    } else {
                        int found = 0;
                        for(auto target : pts[op].first){
                            found += mergePTAPairs(pts[V], local_pts[target]);
                        }
                        if(!found){
                            pts[V].second.unknown = true;
                        }
                    }
                } else if (llvm::isa <llvm::AllocaInst> (*I)){
                    pts[V].first.insert(LLVMPointer(V, Offset(0)));
                } else if (llvm::isa <llvm::CastInst> (*I)){
                    pts[V].first.insert(LLVMPointer(I->getOperand(0), Offset(0)));
                } else if (llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(&*I)){
                    // Not sure whether we want the lone memory locations present in the PTA or not
                    auto func = CI->getCalledFunction();
                    if(func == nullptr){
                        pts[V].second.unknown = true;
                        continue;
                    }
                    llvm::Value *F = llvm::dyn_cast<llvm::Value>(func);
                    if (local_pts.count(LLVMPointer(V, Offset(0)))){
                        pts[V].first.insert(LLVMPointer(V, Offset(0)));
                    } else if (returns.count(F)){
                        mergePTAPairs(pts[V], returns[F]);
                    } else {
                        continue;
                    }
                } else if (llvm::isa <llvm::ReturnInst> (*I)){
                    if (I->getNumOperands() == 0){
                        pts[V].second.unknown = true;
                        continue;
                    }
                    llvm::Value *op = I->getOperand(0);
                    if (pts.count(op)){
                        llvm::Value *F = llvm::dyn_cast<llvm::Value>(I->getFunction());
                        mergePTAPairs(returns[F], pts[op]);
                    }
                    continue;
                } else if (llvm::GetElementPtrInst *GEPI = llvm::dyn_cast<llvm::GetElementPtrInst>(&*I)){
                    if (!GEPI->hasAllConstantIndices()){
                        llvm::Value *op = I->getOperand(0);
                        for (auto dg_ptr : pts[op].first){
                            llvm::Value *target = dg_ptr.value;
                            pts[V].first.insert(LLVMPointer(target, Offset::UNKNOWN));
                        }
                        continue;
                    } 
                    llvm::Value *op = I->getOperand(0);
                    llvm::APInt offset(32, 0, true);
                    GEPI->accumulateConstantOffset(_module->getDataLayout(), offset);
                    for (auto dg_ptr : pts[op].first){
                        llvm::Value *target = dg_ptr.value;
                        int base_offset = *dg_ptr.offset;
                        /*
                        llvm::errs() << "getelementptr looking for: " << *target << "\n";
                        llvm::errs() << "base offset: " << base_offset << "\n";
                        llvm::errs() << "offset: " << offset << "\n";
                        */
                        LLVMPointer ptr(target, Offset(base_offset + offset.getSExtValue()));
                        pts[V].first.insert(ptr);
                        /*
                        if (local_pts.count(ptr)){
                            mergePTAPairs(pts[V], local_pts[ptr]);
                        } else {
                            pts[V].first.insert(ptr);
                        }
                        */
                    }
                    if (offset.getSExtValue() == 0){
                        if (pts[op].second.null){
                            pts[V].second.null = true;
                        }
                        if (pts[op].second.nullWithOffset){
                            pts[V].second.nullWithOffset = true;
                        }
                        if (pts[op].second.unknown){
                            pts[V].second.unknown = true;
                        }
                    } else {
                        if (pts[op].second.null){
                            pts[V].second.nullWithOffset = true;
                        }
                        if (pts[op].second.unknown || pts[op].second.nullWithOffset){
                            pts[V].second.unknown = true;
                        }
                    }
                } else if (llvm::isa <llvm::StoreInst> (*I)){
                    continue;
                } else if (llvm::isa <llvm::BranchInst> (*I)){
                    continue;
                } else if (llvm::isa <llvm::ICmpInst> (*I)){
                    continue;
                } else if (llvm::isa <llvm::UnreachableInst> (*I)){
                    continue;
                } else if (I->isUnaryOp() || I->isBinaryOp()){
                    for (unsigned i = 0; i < I->getNumOperands(); i++){
                        llvm::Value *op = I->getOperand(i);
                        if (pts.count(op)){
                            mergePTAPairs(pts[V], pts[op]);
                        }
                    }
                    continue;
                } else {
                    llvm::errs() << "Unknown type of instruction? " << *I  << "\n";
                    continue;
                }
                /*
                for(auto pt : pts[V].first){
                    llvm::errs() << "-> " << *pt.value << "\n";
                }
                SMGPTAFlags& flags = pts[V].second;
                if (flags.null){
                    llvm::errs() << "                 ->  null" << "\n";
                }
                if (flags.nullWithOffset){
                    llvm::errs() << "                 ->  null + ?" << "\n";
                }
                if (flags.unknown){
                    llvm::errs() << "                 ->  unknown" << "\n";
                }
                if (flags.invalidated){
                    llvm::errs() << "                 ->  invalidated" << "\n";
                }
                */
            }
        }
        return pts;
    }

    /*
    void smg_pts_merge(SMGPTA &target, SMGPTA source){
        // SMGPTA = std::unordered_map<const llvm::Value *, std::pair<SMGPTASet, SMGPTAFlags>>;
        for (auto sourcePTSPair : source){
            if(target.count(sourcePTSPair.first)){
                SMGPTASet& values = sourcePTSPair.second.first;
                SMGPTAFlags& flags = sourcePTSPair.second.second;
                for (auto ptPair : values){
                    target[sourcePTSPair.first].first.insert(ptPair);
                }
                merge_flags(&target[sourcePTSPair.first].second, flags);
            } else {
                target[sourcePTSPair.first] = sourcePTSPair.second;
            }
        }
    }
    */

    bool is_subset(SMGPTA subset, SMGPTA superset){
        for (auto subsetEntry : subset){
            if (superset.count(subsetEntry.first) == 0){
                return false;
            }
            SMGPTASet subsetValues = subsetEntry.second.first;
            SMGPTAFlags subsetFlags = subsetEntry.second.second;

            SMGPTAPair supersetPair = superset[subsetEntry.first];
            SMGPTASet supersetValues = supersetPair.first;
            SMGPTAFlags supersetFlags = supersetPair.second;
            if ((subsetValues != supersetValues || subsetFlags != supersetFlags) && (subsetValues.size() || count_flags(subsetFlags))){
                return false;
            }
        }
        return true;
    }

    void tryToSaveSemiKilledVariables(SMGPTA &first, SMGPTA second){
        for (auto PTPair : second){
            auto target = PTPair.first; 
            if (first.count(target) && first[target].first.size() == 0 && count_flags(first[target].second) == 0){ // still present but empty
                mergePTAPairs(first[target], PTPair.second);
            }
        }
    }

    void filterGlobalPTA(GlobalSMGPTA &target, GlobalSMGPTA source){
        auto previous_it = source.begin();
        SMGPTA previous_pts = previous_it->second;
        target[previous_it->first] = previous_pts;
        for (auto codelocPTAPair : source){
            CodeLoc codeloc = codelocPTAPair.first;
            SMGPTA local_pts = codelocPTAPair.second;
            if (!is_subset(local_pts, previous_pts)){
                //llvm::errs() << codeloc.first << ", " << codeloc.second << " prej neni subset" << "\n";
                tryToSaveSemiKilledVariables(local_pts, previous_pts);
                target[codeloc] = local_pts;
                previous_pts = local_pts;
            /*
            } else {
                llvm::errs() << codeloc.first << ", " << codeloc.second << " prej je subset" << "\n";
            */
            }
        }
    }

    void globalPTAAdd(GlobalSMGPTA &target, SMGPTA source, CodeLoc codeloc){
        //llvm::errs() << "Pridavam: " << codeloc.first << ", " << codeloc.second << " o velikosti" << source.size() << "\n";
        if (!source.size()){
            return;
        }

        if (target.count(codeloc) && is_subset(source, target[codeloc])){
            return;
        }

        if (target.count(codeloc)){
            /*
            llvm::errs() << "merguju!\n!\n!\n!\n!" << "\n";
            dumpSMGPTAPointsTo(target[codeloc]);
            llvm::errs() << "\nZa:" << "\n";
            dumpSMGPTAPointsTo(source);
            */
            mergeSMGPTAs(target[codeloc], source);
        } else {
            target[codeloc] = source;
        }
    }

    bool run() override {
        DBG_SECTION_BEGIN(pta, "Running SMG pointer analysis");
        llvm::errs() << "Running SMG pointer analysis" << "\n";
        std::string smg_prefix = "smg-";
        std::string smg_suffix = ".json";
        GlobalSMGPTA global_pta;
        std::set<std::filesystem::path> path_set;

        for (auto &entry : std::filesystem::directory_iterator(_smg_json_dir_path)){
            path_set.insert(entry.path());
            // TODO: investigate why unsorted paths yield different results
        }
        for (const auto &path : path_set){
            //auto path = entry.path();
            std::string filename = path.filename();
            //llvm::errs() << "File: " << filename << "\n";

            if (!starts_with(filename, smg_prefix) || !ends_with(filename, smg_suffix)){
                continue;
            }

            //llvm::errs() << "Parsing json file: " << path << "\n";
            std::ifstream f(path);
            json smg_json = json::parse(f);
            //llvm::errs() << "Done parsing the json file.\n";

            if (smg_json["metadata"]["func_name"] == "__initGlobalVar"){
                continue;
            }

            //llvm::errs() << "Parsing the json object into SMG.\n";
            SMG smg = SMG(smg_json);
            //llvm::errs() << "Done parsing the SMG.\n";

            //llvm::errs() << "Mapping variables in SMG to LLVM Values.\n";
            smg.mapLLVMValues(_module);
            //llvm::errs() << "Done Mapping variables.\n";
            //smg.dump();

            //llvm::errs() << "Simplifying the SMG into a points-to graph.\n";
            smg.simplify();
            //llvm::errs() << "Done simplifying.\n";

            //smg.dump();
            //dumpSMGPointsTo(smg);

            //llvm::errs() << "Merging information with other loaded SMGs.\n";
            CodeLoc codeloc = {smg_json["metadata"]["line"], smg_json["metadata"]["column"]};

            globalPTAAdd(global_pta, smg.getPointsToSets(), codeloc);
            //llvm::errs() << "Done merging information with other loaded SMGs.\n";
        }
        dumpGlobalSMGPTAPointsTo(global_pta);
        if(global_pta.size() == 0){
            llvm::errs() << "No SMG data provided.\n";
            return false;
        }

        GlobalSMGPTA filtered_global_pta;
        filterGlobalPTA(filtered_global_pta, global_pta);

        dumpGlobalSMGPTAPointsTo(filtered_global_pta);

        //llvm::errs() << "Using SMG points-to information to calculate the DG expected sets.\n";
        _pts = calculateAssistedPTA(filtered_global_pta);

        DBG_SECTION_END(pta, "Done running SMG pointer analysis");
        return true;
    }
};

} // namespace dg

#endif // DG_SMG_POINTER_ANALYSIS_H_
