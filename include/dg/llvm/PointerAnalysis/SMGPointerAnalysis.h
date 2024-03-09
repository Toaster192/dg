#ifndef DG_SMG_POINTER_ANALYSIS_H_
#define DG_SMG_POINTER_ANALYSIS_H_

#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Function.h>
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

class SMGNode{
  public:
    SMGNode(){}

    bool hasValue(){
        return false;
    }

    llvm::Value *getValue(){
        return nullptr;
    }
};

class SMG {
    void load(json &smg_json){
        (void) smg_json;
        return;
    }

  public:
    SMG(json &smg_json){
        load(smg_json);
    }

    SMGNode *getSMGNode(int id){
        (void) id;
        return nullptr;
    }

};

class SMGPointsTo {
    SMG *_smg;
    const llvm::Module *_module{nullptr};
    //size_t _position{0};

    llvm::Value *_getValue(unsigned id) const {
        auto *smgnode = _smg->getSMGNode(id);
        if (smgnode->hasValue())
            return const_cast<llvm::Value *>(smgnode->getValue());

        // for debugging right now
        llvm::errs() << "[SMG] No value in SMG NODE\n";
        //llvm::errs() << *smgnode << "\n";
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
        /*
        std::string str((std::istreambuf_iterator<char>(f)),
                 std::istreambuf_iterator<char>());
        llvm::errs() << "file: " << str << "\n";
        */
        json smg_json = json::parse(f);

        llvm::errs() << "Done parsing the json file" << "\n";

        // TODO figure out if this is still needed?
        //auto *moduleset = LLVMModuleSet::getLLVMModuleSet();

        SMG smg = SMG(smg_json);

        _pta = new SMGPointsTo(&smg, _module);
        _pta->analyze();

        DBG_SECTION_END(pta, "Done running SMG pointer analysis");
        return true;
    }
};

} // namespace dg

#endif // DG_SMG_POINTER_ANALYSIS_H_
