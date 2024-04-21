#ifndef DG_LLVM_POINTER_ANALYSIS_OPTIONS_H_
#define DG_LLVM_POINTER_ANALYSIS_OPTIONS_H_

#include "dg/PointerAnalysis/PointerAnalysisOptions.h"
#include "dg/llvm/LLVMAnalysisOptions.h"

namespace dg {

struct LLVMPointerAnalysisOptions : public LLVMAnalysisOptions,
                                    PointerAnalysisOptions {
    enum class AnalysisType { fi, fs, inv, svf, smg } analysisType{AnalysisType::fi};

    bool threads{false};

    std::string smg_json_dir_path{std::string()};

    bool isFS() const { return analysisType == AnalysisType::fs; }
    bool isFSInv() const { return analysisType == AnalysisType::inv; }
    bool isFI() const { return analysisType == AnalysisType::fi; }
    bool isSVF() const { return analysisType == AnalysisType::svf; }
    bool isSMG() const { return analysisType == AnalysisType::smg; }
};

} // namespace dg

#endif
