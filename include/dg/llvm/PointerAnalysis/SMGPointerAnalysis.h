#ifndef DG_SMG_POINTER_ANALYSIS_H_
#define DG_SMG_POINTER_ANALYSIS_H_
#include <variant>
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

#include "dg/util/debug.h"

#include "json.hpp"

namespace dg {

using json = nlohmann::json;

using pta::Pointer;
using pta::PointerGraph;
using pta::LLVMPointerGraphBuilder;

class SMGGenericObject{
  public:
    int id;
    std::string label;

    SMGGenericObject(){}

    virtual ~SMGGenericObject() = default;

    bool operator==(const SMGGenericObject& other) const {return (id == other.id);}
};

void from_json(const json& j, SMGGenericObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);
}

class SMGVarObject : public SMGGenericObject{
  public:
    std::string varName;
    int varUID;
    std::string varLocFile;
    std::string varLocInsn;
    int varLocLine;
    int varLocColumn;
    llvm::Value* llvmVal{nullptr};

    SMGVarObject(){}
};

void from_json(const json& j, SMGVarObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);

    json v = j.at("value").at("var");
    v.at("name").get_to(o.varName);
    v.at("uid").get_to(o.varUID);

    json l = j.at("value").at("loc");
    l.at("file").get_to(o.varLocFile);
    l.at("line").get_to(o.varLocLine);
    // TODO: remove
    // compensate for the extra import in the predator input source code
    o.varLocLine = o.varLocLine -1;
    l.at("column").get_to(o.varLocColumn);
    l.at("insn").get_to(o.varLocInsn);

    o.llvmVal = nullptr;
}

class SMGRegionObject : public SMGGenericObject{
  public:
    int size_high;
    int size_low;
    // ignore value for now?

    SMGRegionObject(){}
};

void from_json(const json& j, SMGRegionObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);

    j.at("size_low").get_to(o.size_low);
    j.at("size_high").get_to(o.size_high);
}

class SMGEmptyObject : public SMGGenericObject{
  public:
    int size;
    std::string placement;
    // ignore value for now?

    SMGEmptyObject(){}
};

void from_json(const json& j, SMGEmptyObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);

    if (j.find("size") != j.end()){
        j.at("size").get_to(o.size);
    }
    if (j.find("placement") != j.end()){
        j.at("placement").get_to(o.placement);
    }
}


typedef std::variant<SMGGenericObject, SMGVarObject, SMGRegionObject, SMGEmptyObject> SMGSimpleObjectTypeVariant;

class SMGRegionCompositeObject : public SMGGenericObject{
  public:
    std::vector<SMGSimpleObjectTypeVariant> objects;

    SMGRegionCompositeObject(){}
};

void from_json(const json& j, SMGRegionCompositeObject& c) {
    j.at("id").get_to(c.id);
    j.at("label").get_to(c.label);
    for (auto o : j["objects"]){
        if (o["label"] == "empty"){
            c.objects.push_back(o.template get<SMGEmptyObject>());
        } else if (o["label"] == "SC_ON_STACK" || o["label"] == "SC_ON_HEAP"){
            c.objects.push_back(o.template get<SMGRegionObject>());
        } else {
            llvm::errs() << "unknown object label: " << o["label"].template get<std::string>() << "\n";
        }
    }
}

class SMGValue{
  public:
    int id;
    std::string label;
    int obj{0};
    int offset_low{0};
    int offset_high{0};
    bool lonely{false};
    // Don't care about lonely, value, iid?

    std::string memoryLocation;
    int size_high;
    int size_low;
    bool is_var{false};
    SMGVarObject var;

    SMGValue(){}

    bool operator==(const SMGValue& other) const {return (id == other.id);}
};

void from_json(const json& j, SMGValue& v) {
    j.at("id").get_to(v.id);
    j.at("label").get_to(v.label);
    if (j.find("obj") != j.end()){
        j.at("obj").get_to(v.obj);
    }
    if (j.find("offset_low") != j.end()){
        j.at("offset_low").get_to(v.offset_low);
    }
    if (j.find("offset_high") != j.end()){
        j.at("offset_high").get_to(v.offset_high);
    }
    if (j.find("lonely") != j.end()){
        j.at("lonely").get_to(v.lonely);
    }

    v.is_var = false;
}

class SMGEdge{
  public:
    int from;
    int to;
    std::string label;
    int off{0};

    SMGEdge(){}

    bool operator==(const SMGEdge& other) const {return (from == other.from && to == other.to);}
};

void from_json(const json& j, SMGEdge& e) {
    j.at("from").get_to(e.from);
    j.at("to").get_to(e.to);
    j.at("label").get_to(e.label);
    if (j.find("off") != j.end()){
        j.at("off").get_to(e.off);
    }
}

typedef std::variant<SMGGenericObject, SMGVarObject, SMGRegionObject, SMGEmptyObject, SMGRegionCompositeObject> SMGObjectTypeVariant;

int objectVariantGetId(SMGObjectTypeVariant obj){
    // TODO simplify this using templates or something
    if (std::holds_alternative<SMGRegionCompositeObject>(obj)){
        return std::get<SMGRegionCompositeObject>(obj).id;
    } else if (std::holds_alternative<SMGRegionObject>(obj)){
        return std::get<SMGRegionObject>(obj).id;
    } else if (std::holds_alternative<SMGVarObject>(obj)){
        return std::get<SMGVarObject>(obj).id;
    } else if (std::holds_alternative<SMGEmptyObject>(obj)){
        return std::get<SMGEmptyObject>(obj).id;
    } else if (std::holds_alternative<SMGGenericObject>(obj)){
        return std::get<SMGGenericObject>(obj).id;
    } else {
        llvm::errs() << "Unknown object found in objectVariantGetId" << "\n";
        return -1;
    }
}

std::vector<SMGObjectTypeVariant>::iterator getObjectById(int id, std::vector<SMGObjectTypeVariant> &objects){
    std::vector<SMGObjectTypeVariant>::iterator it = objects.begin();
    while (it != objects.end()){
        if (objectVariantGetId(*it) == id){
            break;
        }
        ++it;
    }
    return it;
}

std::optional<SMGObjectTypeVariant> convertSMGObjectVariant(SMGSimpleObjectTypeVariant obj){
    if (std::holds_alternative<SMGRegionObject>(obj)){
        return std::get<SMGRegionObject>(obj);
    } else if (std::holds_alternative<SMGVarObject>(obj)){
        return std::get<SMGVarObject>(obj);
    } else if (std::holds_alternative<SMGEmptyObject>(obj)){
        return std::get<SMGEmptyObject>(obj);
    } else if (std::holds_alternative<SMGGenericObject>(obj)){
        return std::get<SMGGenericObject>(obj);
    } else {
        llvm::errs() << "Unknown type in convertSMGObjectVariant" << "\n";
        return {};
    }
}


class SMG {
    public:
    std::vector<SMGValue> values;
    std::vector<SMGObjectTypeVariant> objects;
    std::vector<SMGEdge> edges;

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
            if (o["label"] == "region"){
                objects.push_back(o.template get<SMGRegionCompositeObject>());
            } else {
                llvm::errs() << "unknown composite object label: " << o["label"].template get<std::string>() << "\n";
            }
        }

        return;
    }

  public:
    SMG(json &smg_json){
        load(smg_json);
        llvm::errs() << "Loaded " << values.size() << " values, " << objects.size() << " objects and " << edges.size() << " edges."  << "\n";
    }

    std::unordered_map<llvm::Value *, SMGValue *> getValueMap(){
        std::unordered_map<llvm::Value *, SMGValue *> valueMap;
        for (auto &val : values){
            if (val.is_var){
                valueMap[val.var.llvmVal] = &val;
            }
        }
        return valueMap;
    }

    std::unordered_map<llvm::Value *, std::vector<std::pair<llvm::Value*, int>>> getPointsToSets(){
        std::unordered_map<llvm::Value *, std::vector<std::pair<llvm::Value*, int>>> pointsToSets;
        std::unordered_map<llvm::Value *, SMGValue *> valueMap = getValueMap();
        for (auto mapping : valueMap){
            std::vector<std::pair<llvm::Value*, int>> PTSet;
            llvm::errs() << "Getting pointsTo for " << *mapping.first << "\n";
            for (auto val : values){
                if (hasEdge(mapping.second->id, val.id) && val.is_var){
                    PTSet.push_back(std::make_pair(val.var.llvmVal, getEdgeOffset(mapping.second->id, val.id)));
                    llvm::errs() << "Adding into points to set " << *val.var.llvmVal << "\n";
                }
            }
            // Unsure how much this makes sense
            PTSet.push_back(std::make_pair(mapping.first, 0));
            pointsToSets[mapping.first] = PTSet;
        }
        return pointsToSets;
    }

    void mapLLVMValues(const llvm::Module *m, std::string func_name){
        std::vector<SMGVarObject*> vars;
        for (auto &obj : objects){
            if (std::holds_alternative<SMGVarObject>(obj)){
                SMGVarObject &var = std::get<SMGVarObject>(obj);
                vars.push_back(&var);
            }
        }
        for (auto &F : *m){
            if (func_name != "" && F.getName() != func_name){
                continue;
            }
            for (llvm::const_inst_iterator I = llvm::inst_begin(F), E = llvm::inst_end(F); I != E; ++I){
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
            }
        }
        if (vars.size()){
            llvm::errs() << "Not every SMG variable mapped!" << "\n";
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
            if (val.label == "int" || val.label == "int_range" || val.label == "real" || val.label == "str"){
                values.erase(val_it);
            } else {
                ++val_it;
            }
        }

        llvm::errs() << "Merging values (" << values.size() << ") with variable and region objects (total objects: " << objects.size() << ")\n";
        // merge values with variables
        for (auto var : varObjects){
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

                    edges.erase(std::find_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id && e.from == val_id;}));
                    objects.erase(it);
                    edges.erase(std::find_if(edges.begin(), edges.end(), [&val_id](SMGEdge e){return e.to == val_id && e.from == val_id;}));

                    objects.erase(std::find_if(objects.begin(), objects.end(), [&var_id](SMGObjectTypeVariant o){return objectVariantGetId(o) == var_id;}));
                    break;
                }
            }
        }
        llvm::errs() << "Merging values (" << values.size() << ") with region objects (total objects: " << objects.size() << ")\n";
        // merge values with memory locations
        for (auto &val : values){
            if (val.lonely){
                llvm::errs() << "Found lonely value: " << val.id << ", label: " << val.label << "\n";
                continue;
            }
            if (val.is_var){
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
            edges.erase(std::find_if(edges.begin(), edges.end(), [&obj_id,&val_id](SMGEdge e){return e.to == obj_id && e.from == val_id;}));
        }
        llvm::errs() << "Done merging values (" << values.size() << ")  objects (" << objects.size() << ")\n";
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
            objects.erase(std::find_if(objects.begin(), objects.end(), [&empty](SMGObjectTypeVariant o){return objectVariantGetId(o) == empty.id;}));
        }
        llvm::errs() << "Done removing empty objects at " << objects.size() << ".\n";
    }

    void filterEdges(){
         
    }

    void simplify(){
        simplifyCompositeObjects();
        simplifyValues();
        simplifyEmptyObjects();
        filterEdges();
        llvm::errs() << "Simplified to " << values.size() << " values, "
                     << objects.size() << " objects, "
                     << edges.size() << " edges.\n";
    }
    /*
    SMGNode *getSMGNode(int id){
        (void) id;
        return nullptr;
    }
    */

};
class SMGPointsTo {
    const SMG *_smg{nullptr};
    const llvm::Module *_module{nullptr};

  public:
    SMGPointsTo(const SMG *smg, const llvm::Module *M) : _smg(smg), _module(M) {}

};
///
// Integration of pointer analysis from predator SMG
class SMGPointerAnalysis : public LLVMPointerAnalysis {
    const llvm::Module *_module{nullptr};
    DGLLVMPointerAnalysis *_dg_pta;
    SMGPointsTo *_pta;
    const char *_smg_json_filename{nullptr};

    /*
    PointsTo &getUnknownPTSet() const {
        static PointsTo _unknownPTSet;
        if (_unknownPTSet.empty())
            _unknownPTSet.set(_pta->getSMG()->getBlackHoleNode());
        return _unknownPTSet;
    }

    */
    LLVMPointsToSet mapSMGPointsTo() {
        return nullptr;
    }

  public:
    SMGPointerAnalysis(const llvm::Module *M,
                       const LLVMPointerAnalysisOptions &opts)
            : LLVMPointerAnalysis(opts), _module(M), _smg_json_filename(opts.smg_json_filename.c_str()) {
        LLVMPointerAnalysisOptions newOpts = opts;
        newOpts.analysisType = LLVMPointerAnalysisOptions::AnalysisType::fi;
        _dg_pta = new DGLLVMPointerAnalysis(M, newOpts);
    }

    ~SMGPointerAnalysis() override {
        // _svfModule overtook the ownership of llvm::Module,
        // we must re-take it to avoid double free
        // TODO figure out if this is still needed?
        //LLVMModuleSet::releaseLLVMModuleSet();
    }

    bool hasPointsTo(const llvm::Value *val) override {
        /*
        SMG *smg = _pta->getSMG();
        auto pts = _pta->getPts(smg->getValueNode(val));
        return !pts.empty();
        */
        return _dg_pta->hasPointsTo(val);
    }

    ///
    // Get the points-to information for the given LLVM value.
    // The return object has methods begin(), end() that can be used
    // for iteration over (llvm::Value *, Offset) pairs of the
    // points-to set. Moreover, the object has methods hasUnknown()
    // and hasNull() that reflect whether the points-to set of the
    // LLVM value contains unknown element of null.
    LLVMPointsToSet getLLVMPointsTo(const llvm::Value *val) override {
        /*
        SMG *smg = _pta->getSMG();
        auto pts = _pta->getPts(smg->getValueNode(val));
        return mapSMGPointsTo();
        */
        return _dg_pta->getLLVMPointsTo(val);
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
        return _dg_pta->getLLVMPointsToChecked(val);
    }

    std::vector<std::pair<PSNode*, int>> mapPointsToToPSNodes(LLVMPointerGraphBuilder *builder, std::vector<std::pair<llvm::Value*, int>> pointsToSet){
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
        std::unordered_map<llvm::Value *, std::vector<std::pair<llvm::Value*, int>>> pointsToSets = smg->getPointsToSets();
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
                        auto operand_nodes = builder->getNodes(operand);
                        for (auto node : *operand_nodes){
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

    bool run() override {
        DBG_SECTION_BEGIN(pta, "Running SMG pointer analysis");
        llvm::errs() << "Running SMG pointer analysis" << "\n";

        llvm::errs() << "Parsing json file: " << _smg_json_filename << "\n";
        std::ifstream f(_smg_json_filename);
        json smg_json = json::parse(f);
        llvm::errs() << "Done parsing the json file" << "\n";

        llvm::errs() << "Parsing the json object into SMG" << "\n";
        SMG *smg = new SMG(smg_json);
        llvm::errs() << "Done parsing the SMG" << "\n";

        llvm::errs() << "Mapping variables in SMG to LLVM Values" << "\n";
        smg->mapLLVMValues(_module, smg_json["metadata"]["func_name"]);
        llvm::errs() << "Done Mapping variables" << "\n";

        llvm::errs() << "Simplifying the SMG into a points-to graph" << "\n";
        smg->simplify();
        llvm::errs() << "Done simplifying" << "\n";

        llvm::errs() << "Initialising a DG points-to object (running DG pta)" << "\n";
        //_pta = new SMGPointsTo(smg, _module);
        _dg_pta->run();
        llvm::errs() << "Done initialising a DG points-to object" << "\n";

        llvm::errs() << "Filtering points-to sets" << "\n";
        PointerGraph *pg = _dg_pta->getPS();
        LLVMPointerGraphBuilder *builder = _dg_pta->getBuilder();
        filterPointerGraph(pg, builder, smg);

        DBG_SECTION_END(pta, "Done running SMG pointer analysis");
        return true;
    }
};

} // namespace dg

#endif // DG_SMG_POINTER_ANALYSIS_H_
