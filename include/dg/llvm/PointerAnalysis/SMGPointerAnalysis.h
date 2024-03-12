#ifndef DG_SMG_POINTER_ANALYSIS_H_
#define DG_SMG_POINTER_ANALYSIS_H_

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>

#include "dg/PointerAnalysis/Pointer.h"

#include "dg/llvm/PointerAnalysis/LLVMPointerAnalysisOptions.h"
#include "dg/llvm/PointerAnalysis/LLVMPointsToSet.h"
#include "dg/llvm/PointerAnalysis/PointerAnalysis.h"

#include "dg/util/debug.h"

#include "json.hpp"

    namespace dg {

    using json = nlohmann::json;

    using pta::Pointer;

    class SMGValue{
      public:
        int id;
        std::string label;
        int obj{0};
        int offset_low{0};
        int offset_high{0};
        // Don't care about lonely, value, iid?

        SMGValue(){}
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
    }

    class SMGObject{
      public:
        int id;
        std::string label;

        SMGObject(){}
    };

    void from_json(const json& j, SMGObject& o) {
        j.at("id").get_to(o.id);
        j.at("label").get_to(o.label);
    }

    class SMGVarObject : public SMGObject{
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
        l.at("column").get_to(o.varLocColumn);
        l.at("insn").get_to(o.varLocInsn);
    }

    class SMGRegionObject : public SMGObject{
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

    class SMGRegionCompositeObject : public SMGObject{
      public:
        std::vector<SMGObject> objects;

        SMGRegionCompositeObject(){}
    };

    void from_json(const json& j, SMGRegionCompositeObject& c) {
        j.at("id").get_to(c.id);
        j.at("label").get_to(c.label);
        for (auto o : j["objects"]){
            if (o["label"] == "empty"){
                c.objects.push_back(o.template get<SMGObject>());
            } else if (o["label"] == "SC_ON_STACK" || o["label"] == "SC_ON_HEAP"){
                c.objects.push_back(o.template get<SMGRegionObject>());
            } else {
                llvm::errs() << "unknown object label: " << o["label"].template get<std::string>() << "\n";
            }
        }
    }

    class SMGEdge{
      public:
        int from;
        int to;
        std::string label;
        int off{0};

        SMGEdge(){}
    };

    void from_json(const json& j, SMGEdge& e) {
        j.at("from").get_to(e.from);
        j.at("to").get_to(e.to);
        j.at("label").get_to(e.label);
        if (j.find("off") != j.end()){
            j.at("off").get_to(e.off);
        }
    }

    class SMG {
        std::vector<SMGValue> values;
        std::vector<SMGObject> objects;
        std::vector<SMGEdge> edges;
        std::vector<SMGObject> compositeObjects;

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
                    compositeObjects.push_back(o.template get<SMGRegionCompositeObject>());
                } else {
                    llvm::errs() << "unknown composite object label: " << o["label"].template get<std::string>() << "\n";
                }
            }

            return;
        }

      public:
        SMG(json &smg_json){
            load(smg_json);
            llvm::errs() << "Loaded " << values.size() << " values, " << objects.size() << " objects, " << edges.size() << " edges and " << compositeObjects.size() << " composite objects." << "\n";
        }

        /*
        SMGNode *getSMGNode(int id){
            (void) id;
            return nullptr;
        }
        */

    };

    class SMGPointsTo {
        SMG *_smg;
        const llvm::Module *_module{nullptr};
        //size_t _position{0};

        llvm::Value *_getValue(unsigned id) const {
            /*
            auto *smgnode = _smg->getSMGNode(id);
            if (smgnode->hasValue())
                return const_cast<llvm::Value *>(smgnode->getValue());

            // for debugging right now
            llvm::errs() << "[SMG] No value in SMG NODE\n";
            //llvm::errs() << *smgnode << "\n";
            */
            (void) id;
            (void) _module;
            return nullptr;
        }

        /*
        void _findNextReal() {
            while (it != PTSet.end()) {
                if (_smg->getSMGNode(*it)->hasValue())
                    break;
                // else
                //     llvm::errs() << "no val" << *_smg->getSMGNode(*it) << "\n";

                ++it;
                ++_position;
            }
        }
        */

      public:
        SMGPointsTo(SMG *smg, const llvm::Module *module) : _smg(smg), _module(module) {}

        SMG *getSMG(){
            return _smg;
        }

        LLVMPointsToSet toLLVMPointsToSet() {
            return nullptr;
        }

        void analyze(){};

        /*
        bool hasUnknown() const {
            return PTSet.test(_smg->getBlackHoleNode());
        }

        bool hasNull() const { return PTSet.test(_smg->getNullPtr()); }

        bool hasNullWithOffset() const {
            // we are field-insensitive now...
            return hasNull();
        }

        bool hasInvalidated() const { return false; }
        size_t size() const { return PTSet.count(); }

        LLVMPointer get() const {
            assert(it != PTSet.end() && "Dereferenced end() iterator");
            return {_getValue(*it), Offset::UNKNOWN};
        }
        */
    };

    ///
    // Integration of pointer analysis from predator SMG
    class SMGPointerAnalysis : public LLVMPointerAnalysis {
        const llvm::Module *_module{nullptr};
        SMGPointsTo *_pta{nullptr};
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
            return _pta->toLLVMPointsToSet();
        }

      public:
        SMGPointerAnalysis(const llvm::Module *M,
                           const LLVMPointerAnalysisOptions &opts)
                : LLVMPointerAnalysis(opts), _module(M), _smg_json_filename(opts.smg_json_filename.c_str()) {}

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
            (void) val;
            return false;
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
            */
            (void) val;
            return mapSMGPointsTo();
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
            */
            (void) val;
            return {false, mapSMGPointsTo()};
        }

        bool run() override {
            DBG_SECTION_BEGIN(pta, "Running SMG pointer analysis");
            llvm::errs() << "Running SMG pointer analysis" << "\n";

            llvm::errs() << "Parsing json file: " << _smg_json_filename << "\n";

            std::ifstream f(_smg_json_filename);
            json smg_json = json::parse(f);

            llvm::errs() << "Done parsing the json file" << "\n";

            // TODO figure out if this is still needed?
            //auto *moduleset = LLVMModuleSet::getLLVMModuleSet();

        llvm::errs() << "Parsing the json object into SMG" << "\n";

        SMG smg = SMG(smg_json);

        llvm::errs() << "Done parsing the SMG" << "\n";

        _pta = new SMGPointsTo(&smg, _module);
        _pta->analyze();

        DBG_SECTION_END(pta, "Done running SMG pointer analysis");
        return true;
    }
};

} // namespace dg

#endif // DG_SMG_POINTER_ANALYSIS_H_
