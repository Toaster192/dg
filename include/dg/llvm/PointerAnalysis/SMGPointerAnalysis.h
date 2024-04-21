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
#include "llvm/IR/InstIterator.h"
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

namespace fs = std::filesystem;

using json = nlohmann::json;

namespace dg {

using pta::Pointer;
using pta::PointerGraph;
using pta::LLVMPointerGraphBuilder;

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
        llvm::errs() << "Parsing SMG values" << "\n";
        for (auto v : smg_json["values"]){
            values.push_back(v.template get<SMGValue>());
        }
        llvm::errs() << "Parsing SMG objects" << "\n";
        for (auto o : smg_json["objects"]){
            if (o["label"] == "field"){
                objects.push_back(o.template get<SMGVarObject>());
            } else {
                llvm::errs() << "unknown object label: " << o["label"].template get<std::string>()  << "\n";
            }
        }
        llvm::errs() << "Parsing SMG edges" << "\n";
        for (auto e : smg_json["edges"]){
            edges.push_back(e.template get<SMGEdge>());
        }
        llvm::errs() << "Parsing SMG composite objects" << "\n";
        for (auto o : smg_json["compositeObjects"]){
            std::string label = o["label"].template get<std::string>();
            if (label == "region"){
                objects.push_back(o.template get<SMGRegionCompositeObject>());
            } else if (label == "DLS" || label == "SLS"){
                llvm::errs() << "found DLS/SLS object label: " << label << "\n";
            } else {
                llvm::errs() << "unknown composite object label: " << label << "\n";
            }
        }

        return;
    }

    int kill_duplicate_memory_values(){
        // TODO do this properly by checking if the obj is referenced in multiple values?
        // these should probably be merged instead? just their offsets somehow saved
        int killed = 0;
        auto it = values.begin();
        while (it != values.end()){
            auto val = *it;
            if (val.is_mem && val.offset_low != 0) {
                int val_id = val.id;
                edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.from == val_id || e.to == val_id;}), edges.end());
                values.erase(it);
                killed++;
            } else {
                ++it;
            }
        }
        return killed;
    }

  public:
    std::vector<SMGValue> values;
    std::vector<SMGObjectTypeVariant> objects;
    std::vector<SMGEdge> edges;

    SMG(json &smg_json){
        load(smg_json);
        llvm::errs() << "Killing duplicate virtual values."  << "\n";
        int killed = kill_duplicate_memory_values();
        llvm::errs() << "Done killing (" << killed << ") duplicate virtual memory values.\n";
        llvm::errs() << "Loaded " << values.size() << " values, " << objects.size() << " objects and " << edges.size() << " edges."  << "\n";
    }

    std::unordered_map<llvm::Value *, SMGValue *> getValueMap(){
        std::unordered_map<llvm::Value *, SMGValue *> valueMap;
        for (auto &val : values){
            if (val.is_var){
                valueMap[val.var.llvmVal] = &val;
            } else if (val.is_mem){
                valueMap[val.llvmVal] = &val;
            } else if (!val.is_null){
                llvm::errs() << "Unknown value found in getValueMap(), id: " << val.id << " \n";
            }
        }
        return valueMap;
    }

    SMGPTA getPointsToSets(){
        SMGPTA pointsToSets;
        std::unordered_map<llvm::Value *, SMGValue *> valueMap = getValueMap();
        for (auto mapping : valueMap){
            SMGPTASet PTSet;
            SMGPTAFlags PTFlags = {false};
            for (auto val : values){
                if (hasEdge(mapping.second->id, val.id)){
                    llvm::Value* value = nullptr;
                    if (val.is_var){
                        value = val.var.llvmVal; 
                    } else if (val.is_mem){
                        value = val.llvmVal; 
                    } else if (val.is_null){
                        PTFlags.null = true;
                        continue;
                    } else {
                        llvm::errs() << "Unknown value found in getPointsToSets(), id: " << val.id << " \n";
                    }
                    Offset offset(getEdgeOffset(mapping.second->id, val.id));
                    PTSet.insert(LLVMPointer(value, offset));
                }
            }
            // Unsure how much this makes sense
            // PTSet.push_back(std::make_pair(mapping.first, 0));
            pointsToSets[mapping.first] = std::make_pair(PTSet, PTFlags );
        }
        return pointsToSets;
    }

    void replaceGlobals(){
        std::vector<SMGVarObject*> globals;
        for (auto &obj : objects){
            if (std::holds_alternative<SMGVarObject>(obj)){
                SMGVarObject &var = std::get<SMGVarObject>(obj);
                if (var.llvmVal && llvm::isa<llvm::GlobalVariable>(var.llvmVal)){
                    globals.push_back(&var);
                }
            }
        }
        for (auto global : globals){
            // TODO: maybe fill out some other SMGValue attributes?
            SMGValue *val = new SMGValue();
            int var_id = global->id;
            val->id = var_id;
            val->var = *global;
            val->is_var = true;
            val->llvmVal = global->llvmVal;
            objects.erase(std::remove_if(objects.begin(), objects.end(), [&var_id](SMGObjectTypeVariant o){return objectVariantGetId(o) == var_id;}), objects.end());
            values.push_back(*val);
        }
        return;
    }

    void mapLLVMValues(llvm::Module *m){
        std::vector<SMGVarObject*> vars;
        for (auto &obj : objects){
            if (std::holds_alternative<SMGVarObject>(obj)){
                SMGVarObject &var = std::get<SMGVarObject>(obj);
                if (var.varName != ""){
                    vars.push_back(&var);
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
                //llvm::errs() << "inst: " << *I  << "\n";
                //llvm::errs() << "operand 0: " << *I->getOperand(0) << "\n";
                llvm::DebugLoc dbg = I->getDebugLoc();
                if (!dbg){
                    continue;
                }
                for (auto &var : vars){
                    if (dbg.getLine() == (unsigned) var->varLocLine && dbg.getCol() == (unsigned) var->varLocColumn){
                        llvm::Metadata *Meta = llvm::cast<llvm::MetadataAsValue>(I->getOperand(0))->getMetadata();
                        // gambing a bit but should be fine
                        //if(llvm::isa<llvm::ValueAsMetadata>(Meta)){
                        llvm::Value *V = llvm::cast <llvm::ValueAsMetadata>(Meta)->getValue();
                        var->llvmVal = V;
                        //llvm::errs() << "val: " << *var->llvmVal  << "\n";
                        auto it = std::find(vars.begin(), vars.end(), var);
                        vars.erase(it);
                        break;
                    }
                }
                for (auto &val : mems){
                    if (dbg.getLine() == (unsigned) val->memLocLine && dbg.getCol() == (unsigned) val->memLocColumn){
                        llvm::Value *V = llvm::dyn_cast<llvm::Value>(&(*I));
                        val->llvmVal = V;
                        //llvm::errs() << "val: " << *val->llvmVal  << "\n";
                        auto it = std::find(mems.begin(), mems.end(), val);
                        mems.erase(it);
                        break;
                    }
                }
            }
        }
        // Map global variables
        std::vector<SMGVarObject*>::iterator it = vars.begin();
        while (it != vars.end()){
            auto var = *it;
            if (auto globVar = m->getGlobalVariable(var->varName)){
                llvm::Value *V = llvm::cast <llvm::Value>(globVar);
                var->llvmVal = V;
                vars.erase(it);
            } else {
                ++it;
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
    }

    std::pair<std::vector<SMGEdge*>, std::vector<SMGEdge*>> getEdgesWithId(int id){
        std::vector<SMGEdge*> toId;
        std::vector<SMGEdge*> fromId;
        for (auto &edge : edges){
            if (edge.to == id){
                toId.push_back(&edge);
            }
            if (edge.from == id){
                fromId.push_back(&edge);
            }
        }
        return std::make_pair(toId, fromId);
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
        llvm::errs() << "Calling getEdgeOffset on non-existent edge" << "\n";
        return 0;
    }

    void removeAnonymousObjects(){
        llvm::errs() << "Removing anonymous objects, total objects: " << objects.size() << "\n";
        auto it = objects.begin();
        while (it != objects.end()){
            auto obj = *it;
            if (std::holds_alternative<SMGVarObject>(obj)){
                SMGVarObject var = std::get<SMGVarObject>(obj);
                if (var.varName == ""){
                    int var_id = var.id;

                    edges.erase(std::remove_if(edges.begin(), edges.end(), [&var_id](SMGEdge e){return e.from == var_id || e.to == var_id;}), edges.end());

                    objects.erase(it);
                    continue;
                }
            }
            ++it;
        }
        llvm::errs() << "Done removing anonymous objects, total objects: " << objects.size() << "\n";
    }

    void simplifyCompositeObjects(){
        llvm::errs() << "Simplifying composite objects, total objects: " << objects.size() << "\n";
        std::vector<SMGObjectTypeVariant> newObjects;
        auto it = objects.begin();
        while (it != objects.end()){
            auto obj = *it;
            if (std::holds_alternative<SMGRegionCompositeObject>(obj)){
                SMGRegionCompositeObject region = std::get<SMGRegionCompositeObject>(obj);
                for (auto &o : region.objects){
                    if (auto converted = convertSMGObjectVariant(o)){
                        newObjects.push_back(*converted);
                    } else {
                        llvm::errs() << "Failed to convert to object variant in composite object " << region.id << "\n";
                    }
                }
                objects.erase(it);
            } else {
                ++it;
            }
        }
        objects.insert(objects.end(), newObjects.begin(), newObjects.end());
        llvm::errs() << "Done simplifying composite objects, new total objects: " << objects.size() << "\n";
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
        llvm::errs() << "Removing non-pointer values (current total: " << values.size() << ")\n";
        auto val_it = values.begin();
        while (val_it != values.end()){
            auto val = *val_it;
            int val_id = val.id;
            // TODO: keep ints?
            if (val.label == "int" || val.label == "int_range" || val.label == "real" || val.label == "str"){
                values.erase(val_it);
                edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id;}), edges.end());
            } else {
                ++val_it;
            }
        }

        llvm::errs() << "Merging values (" << values.size() << ") with variable and region objects (total objects: " << objects.size() << ")\n";
        // merge values with variables
        for (auto var : varObjects){
            if (not var.llvmVal){
                continue;
            }
            for (auto &val : values){
                if(hasEdge(var.id, val.id)){
                    int obj_id = val.obj;
                    int val_id = val.id;
                    int var_id = var.id;

                    auto it = getObjectById(obj_id, objects);
                    if (it == objects.end()){
                        llvm::errs() << "ID " << obj_id << " not found!" << "\n";
                        return;
                    }
                    SMGRegionObject obj = std::get<SMGRegionObject>(*it);

                    val.memoryLocation = obj.label;
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
        llvm::errs() << "Merging values (" << values.size() << ") with region objects (total objects: " << objects.size() << ")\n";
        std::vector<SMGValue*> duplicateValues;
        // merge values with memory locations
        for (auto &val : values){
            if (val.is_var || val.is_null){
                continue;
            }
            if (val.lonely){
                llvm::errs() << "Found lonely value: " << val.id << ", label: " << val.label << "\n";
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
                llvm::errs() << "ID " << obj_id << " not found!" << "\n";
                llvm::errs() << "val: " << val.id << "\n";
                return;
            }
            SMGRegionObject obj = std::get<SMGRegionObject>(*it);

            val.memoryLocation = obj.label;
            val.size_low = obj.size_low;
            val.size_high = obj.size_high;

            objects.erase(it);
            edges.erase(std::remove_if(edges.begin(), edges.end(), [&obj_id,&val_id](SMGEdge e){return e.to == obj_id && e.from == val_id;}), edges.end());
            replaceEdgeTargets(obj_id, val_id);
        }
        if (duplicateValues.size()){
            llvm::errs() << "Merging duplicate values (" << values.size() << ") objects (" << objects.size() << ")\n";
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
        llvm::errs() << "Done merging values (" << values.size() << ") objects (" << objects.size() << ")\n";
    }

    void simplifyEmptyObjects(){
        llvm::errs() << "Finding empty objects in " << objects.size() << " objects." << "\n";
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

        llvm::errs() << "Found " << emptyObjects.size() << " empty objects." << "\n";
        for (SMGEmptyObject &empty : emptyObjects){
            auto connected_edges = getEdgesWithId(empty.id);
            auto to_edges = connected_edges.first;
            auto from_edges = connected_edges.second;

            if (to_edges.size() != 1){
                llvm::errs() << "More than 1 edge pointing at object id " << empty.id << "!\n";
                return;
            }
            SMGEdge to_edge = *to_edges[0];
            for (auto from_edge : from_edges){
                from_edge->from = to_edge.from;
                from_edge->off += to_edge.off;
            }

            edges.erase(std::find(edges.begin(), edges.end(), to_edge));
            objects.erase(std::remove_if(objects.begin(), objects.end(), [&empty](SMGObjectTypeVariant o){return objectVariantGetId(o) == empty.id;}), objects.end());
        }
        llvm::errs() << "Done removing empty objects at " << objects.size() << ".\n";
    }

    void removeUnmergedValues(){
        llvm::errs() << "Removing unmerged values, total values: " << values.size() << "\n";
        auto it = values.begin();
        while (it != values.end()){
            auto val = *it;
            int val_id = val.id;
            if (val.is_mem || val.is_var || val.is_null){
                ++it;
            } else {
                llvm::errs() << "Removing unmerged value: " << val_id << "\n";
                edges.erase(std::remove_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id || e.from == val_id;}), edges.end());
                values.erase(it);
            }
        }
        llvm::errs() << "Done removing unmerged values, total values: " << values.size() << "\n";
    }

    void removeStrandedValues(){
        llvm::errs() << "Removing stranded values, total values: " << values.size() << "\n";
        auto it = values.begin();
        while (it != values.end()){
            auto val = *it;
            auto connected_edges = getEdgesWithId(val.id);
            auto to_edges = connected_edges.first;
            auto from_edges = connected_edges.second;
            if ((to_edges.size() + from_edges.size()) == 0){
                values.erase(it);
            } else {
                ++it;
            }
        }
        llvm::errs() << "Done removing stranded values, total values: " << values.size() << "\n";
    }

    void simplify(){
        replaceGlobals();
        simplifyCompositeObjects();
        simplifyValues();
        simplifyEmptyObjects();
        removeUnmergedValues();
        removeAnonymousObjects();
        removeStrandedValues();
        llvm::errs() << "Simplified to " << values.size() << " values, "
                     << objects.size() << " objects, "
                     << edges.size() << " edges.\n";
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
        flag_count = F.null + F.nullWithOffset + F.unknown + F.invalidated;
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
    SMGPTA _pts;
    SMGPTA _smg_pts;

    /*
    PointsTo &getUnknownPTSet() const {
        static PointsTo _unknownPTSet;
        if (_unknownPTSet.empty())
            _unknownPTSet.set(_pta->getSMG()->getBlackHoleNode());
        return _unknownPTSet;
    }

    */
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
        /*
        SMG *smg = _pta->getSMG();
        auto pts = _pta->getPts(smg->getValueNode(val));
        return !pts.empty();
        */
        //return _dg_pta->hasPointsTo(val);
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
        /*
        SMG *smg = _pta->getSMG();
        auto pts = _pta->getPts(smg->getValueNode(val));
        return {!pts.empty(), mapSMGPointsTo()};
        return {false, mapSMGPointsTo()};
        */
        //return _dg_pta->getLLVMPointsToChecked(val);
        return {hasPointsTo(val), getLLVMPointsTo(val)};
    }

    /*
    std::vector<std::pair<PSNode*, int>> mapPointsToToPSNodes(LLVMPointerGraphBuilder *builder, std::vector<std::pair<const llvm::Value*, int>> pointsToSet){
        std::vector<std::pair<PSNode*, int>> mappedSets;

        for (auto PTPair: pointsToSet){
            if (auto nodes = builder->getNodes(PTPair.first)){
                for (auto node : *nodes){
                    mappedSets.push_back(std::make_pair(node, PTPair.second));
                }
            }
        }
        return mappedSets;
    }

    void filterPointerGraph(PointerGraph *pg, LLVMPointerGraphBuilder *builder, SMG *smg){
        std::unordered_map<const llvm::Value *, std::vector<std::pair<const llvm::Value*, int>>> pointsToSets = smg->getPointsToSets();
        auto nodesMap = builder->getNodesMap();

        for (auto PTSet : pointsToSets){
            std::vector<std::pair<PSNode*, int>> mappedSet = mapPointsToToPSNodes(builder, PTSet.second);
            llvm::errs() << "nodes for: " << *PTSet.first << "\n";
            for (auto & [llvmVal, nodes] : nodesMap){
                if (llvmVal == PTSet.first){
                    for (auto node : nodes){
                        llvm::errs() << "node - "<< *llvmVal << "\n";
                        node->filterPointsTo(mappedSet);
                    }
                }
                if(llvm::isa<llvm::LoadInst>(llvmVal)){
                    const llvm::LoadInst *LI = llvm::cast <llvm::LoadInst>(llvmVal);
                    auto operand = LI->getPointerOperand();
                    if (operand == PTSet.first){
                        //auto operand_nodes = builder->getNodes(operand);
                        for (auto node : nodes){
                            if (node->doesPointsTo(node, 0)){
                                llvm::errs() << "huh\n";
                            }
                            llvm::errs() << "operand node - " << *llvmVal << " - " << node->pointsTo.size() << "\n";
                            llvm::errs() << " - " << mappedSet.size() << "\n";
                            node->filterPointsTo(mappedSet);
                            llvm::errs() << "operand node - " << *llvmVal << " - " << node->pointsTo.size() << "\n";
                        }
                    }
                }
            }
        }
        return;
    }
    */

    void dumpSMG(SMG smg){
        for (auto obj : smg.objects){
            llvm::errs() << "obj: " << objectVariantGetId(obj) << "\n";
        }
        for (auto val : smg.values){
            llvm::errs() << "val: " << val.id << ", ";
            llvm::errs() << val.label << "\n";
        }
        for (auto edge : smg.edges){
            llvm::errs() << "edge: "<<  edge.from << ", " << edge.to << ", " << edge.off << "\n";
        }
    }

    void dumpSMGPointsTo(SMG smg){
        SMGPTA pointsToSets = smg.getPointsToSets();
        dumpSMGPTAPointsTo(pointsToSets);
    }

    void dumpSMGPTAPointsTo(SMGPTA pointsToSets){
        for (auto PTPair : pointsToSets){
            llvm::errs() << "SMG points to info:" << *PTPair.first << "\n";
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

    int mergePTAPairs(SMGPTAPair &target, const SMGPTAPair source){
        int found = 0;
        for(auto pt : source.first){
            target.first.insert(pt);
            found++;
        }
        merge_flags(&target.second, source.second);
        return found;
    }

    SMGPTA calculateAssistedPTA(){
        SMGPTA pts;
        SMGPTA returns;
        for (auto &F : *_module){
            for (llvm::inst_iterator I = llvm::inst_begin(F), E = llvm::inst_end(F); I != E; ++I){
                llvm::errs() << "inst: " << *I  << "\n";
                llvm::Value *V = llvm::dyn_cast<llvm::Value>(&(*I));
                if (llvm::isa <llvm::LoadInst> (*I)){
                    llvm::Value *op = I->getOperand(0);
                    // TODO: Figure out how to use all indexes, not just the first of every set
                    if (_smg_pts.count(op)){
                        mergePTAPairs(pts[V], _smg_pts[op]);
                    } else {
                        int found = 0;
                        for(auto target : pts[op].first){
                            found += mergePTAPairs(pts[V], _smg_pts[target.value]);
                        }
                        if(!found){
                            llvm::errs() << "Fix load instruction pta " << *I  << "\n";
                        }
                    }
                } else if (llvm::isa <llvm::AllocaInst> (*I)){
                    pts[V].first.insert(LLVMPointer(V, Offset(0)));
                } else if (llvm::isa <llvm::CastInst> (*I)){
                    pts[V].first.insert(LLVMPointer(I->getOperand(0), Offset(0)));
                } else if (llvm::CallInst *CI = llvm::dyn_cast<llvm::CallInst>(&*I)){
                    llvm::Value *F = llvm::dyn_cast<llvm::Value>(CI->getCalledFunction());
                    if (_smg_pts.count(V)){
                        pts[V].first.insert(LLVMPointer(V, Offset(0)));
                    } else if (returns.count(F)){
                        mergePTAPairs(pts[V], returns[F]);
                    } else {
                        continue;
                    }
                } else if (llvm::isa <llvm::ReturnInst> (*I)){
                    llvm::Value *op = I->getOperand(0);
                    if (pts.count(op)){
                        llvm::Value *F = llvm::dyn_cast<llvm::Value>(I->getFunction());
                        mergePTAPairs(returns[F], pts[op]);
                    }
                    continue;
                } else if (llvm::isa <llvm::GetElementPtrInst> (*I)){
                    // TODO
                } else if (llvm::isa <llvm::StoreInst> (*I)){
                    continue;
                } else if (llvm::isa <llvm::BranchInst> (*I)){
                    continue;
                } else if (llvm::isa <llvm::ICmpInst> (*I)){
                    continue;
                } else {
                    llvm::errs() << "Unknown type of instruction? " << *I  << "\n";
                    continue;
                }
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
            }
        }
        return pts;
    }

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


    bool run() override {
        DBG_SECTION_BEGIN(pta, "Running SMG pointer analysis");
        llvm::errs() << "Running SMG pointer analysis" << "\n";
        std::string smg_prefix = "./smg-";
        std::string smg_suffix = ".json";
        _smg_pts = SMGPTA();

        for (const auto &entry : fs::directory_iterator(_smg_json_dir_path)){
            std::string filename = entry.path();

            if (!starts_with(filename, smg_prefix) || !ends_with(filename, smg_suffix)){
                continue;
            }

            llvm::errs() << "Parsing json file: " << filename << "\n";
            std::ifstream f(filename);
            json smg_json = json::parse(f);
            llvm::errs() << "Done parsing the json file.\n";

            if (smg_json["metadata"]["func_name"] == "__initGlobalVar"){
                continue;
            }

            llvm::errs() << "Parsing the json object into SMG.\n";
            SMG smg = SMG(smg_json);
            llvm::errs() << "Done parsing the SMG.\n";

            llvm::errs() << "Mapping variables in SMG to LLVM Values.\n";
            smg.mapLLVMValues(_module);
            llvm::errs() << "Done Mapping variables.\n";

            llvm::errs() << "Simplifying the SMG into a points-to graph.\n";
            smg.simplify();
            llvm::errs() << "Done simplifying.\n";

            dumpSMG(smg);
            dumpSMGPointsTo(smg);

            llvm::errs() << "Merging information with other loaded SMGs.\n";
            smg_pts_merge(_smg_pts, smg.getPointsToSets());
            llvm::errs() << "Done merging information with other loaded SMGs.\n";

            dumpSMGPTAPointsTo(_smg_pts);
        }

        llvm::errs() << "Using SMG points-to information to calculate the DG expected sets.\n";
        _pts = calculateAssistedPTA();

        //_pta = new SMGPointsTo(smg, _module);
        /*
        llvm::errs() << "Initialising a DG points-to object (running DG pta)" << "\n";
        _dg_pta->run();
        llvm::errs() << "Done initialising a DG points-to object" << "\n";

        llvm::errs() << "Filtering points-to sets" << "\n";
        PointerGraph *pg = _dg_pta->getPS();
        LLVMPointerGraphBuilder *builder = _dg_pta->getBuilder();
        filterPointerGraph(pg, builder, smg);
        */
        DBG_SECTION_END(pta, "Done running SMG pointer analysis");
        return true;
    }
};

} // namespace dg

#endif // DG_SMG_POINTER_ANALYSIS_H_
