#ifndef DG_SMG_OBJECTS_H_
#define DG_SMG_OBJECTS_H_
#include <set>
#include <string>
#include <variant>
#include <vector>
#include <iostream>
#include <fstream>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include "llvm/IR/InstIterator.h"
#include <llvm/Support/raw_ostream.h>

#include "dg/PointerAnalysis/Pointer.h"

#include "dg/llvm/PointerAnalysis/PointerAnalysis.h"

#include "dg/util/debug.h"

#include "json.hpp"

using json = nlohmann::json;

namespace dg{

using pta::Pointer;

typedef struct {bool null = false; bool nullWithOffset = false; bool unknown = false; bool invalidated = false;} SMGPTAFlags; // TODO provide the invalidated information
using SMGPTASet = std::set<dg::LLVMPointer>;
using SMGPTAPair = std::pair<SMGPTASet, SMGPTAFlags>;
using SMGPTA = std::unordered_map<const llvm::Value *, SMGPTAPair>;


void merge_flags(SMGPTAFlags *target, const SMGPTAFlags source);

class SMGGenericObject{
  public:
    int id;
    std::string label;

    SMGGenericObject(){}

    virtual ~SMGGenericObject() = default;

    bool operator==(const SMGGenericObject& other) const {return (id == other.id);}
};

void from_json(const json& j, SMGGenericObject& o);

class SMGVarObject : public SMGGenericObject{
  public:
    std::string varName{""};
    int varUID;
    std::string varLocFile;
    std::string varLocInsn;
    int varLocLine;
    int varLocColumn;
    llvm::Value* llvmVal{nullptr};

    SMGVarObject(){}
};

void from_json(const json& j, SMGVarObject& o);

class SMGRegionObject : public SMGGenericObject{
  public:
    int size_high;
    int size_low;
    // ignore value for now?

    SMGRegionObject(){}
};

void from_json(const json& j, SMGRegionObject& o);

class SMGEmptyObject : public SMGGenericObject{
  public:
    int size;
    std::string placement;
    // ignore value for now?

    SMGEmptyObject(){}
};

void from_json(const json& j, SMGEmptyObject& o);

typedef std::variant<SMGGenericObject, SMGVarObject, SMGRegionObject, SMGEmptyObject> SMGSimpleObjectTypeVariant;

class SMGRegionCompositeObject : public SMGGenericObject{
  public:
    std::vector<SMGSimpleObjectTypeVariant> objects;

    SMGRegionCompositeObject(){}
};

void from_json(const json& j, SMGRegionCompositeObject& c);

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
    bool is_null{false};
    bool is_var{false};
    SMGVarObject var;
    bool is_mem{false};
    std::string memLocFile;
    int memLocLine;
    int memLocColumn;
    llvm::Value* llvmVal{nullptr};

    SMGValue(){}

    bool operator==(const SMGValue& other) const {return (id == other.id);}
};

void from_json(const json& j, SMGValue& v);

class SMGEdge{
  public:
    int from;
    int to;
    std::string label;
    int off{0};

    SMGEdge(){}

    bool operator==(const SMGEdge& other) const {return (from == other.from && to == other.to);}
};

void from_json(const json& j, SMGEdge& e);

typedef std::variant<SMGGenericObject, SMGVarObject, SMGRegionObject, SMGEmptyObject, SMGRegionCompositeObject> SMGObjectTypeVariant;

int objectVariantGetId(SMGObjectTypeVariant obj);

std::vector<SMGObjectTypeVariant>::iterator getObjectById(int id, std::vector<SMGObjectTypeVariant> &objects);

std::optional<SMGObjectTypeVariant> convertSMGObjectVariant(SMGSimpleObjectTypeVariant obj);

}

#endif // DG_SMG_OBJECTS_H_
