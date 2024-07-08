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

class SMGGenericObject{
  public:
    int id;
    std::string label;

    SMGGenericObject(){}

    virtual ~SMGGenericObject() = default;

    bool operator==(const SMGGenericObject& other) const {return (id == other.id);}
};

inline void from_json(const json& j, SMGGenericObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);
}

class SMGVarObject : public SMGGenericObject{
  public:
    std::string var_name{""};
    int varUID;
    std::string var_loc_file;
    //std::string var_loc_insn;
    int var_loc_line;
    int var_loc_column;
    llvm::Value* llvm_val{nullptr};

    SMGVarObject(){}
};

inline void from_json(const json& j, SMGVarObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);

    json v = j.at("value").at("var");
    if (v.find("name") != v.end()){
        v.at("name").get_to(o.var_name);
    }
    v.at("uid").get_to(o.varUID);

    json l = j.at("value").at("loc");
    l.at("file").get_to(o.var_loc_file);
    l.at("line").get_to(o.var_loc_line);
    l.at("column").get_to(o.var_loc_column);
    /*
    if (l.find("insn") != l.end()){
        l.at("insn").get_to(o.var_loc_insn);
    }
    */

    o.llvm_val = nullptr;
}

class SMGRegionObject : public SMGGenericObject{
  public:
    int size_high;
    int size_low;
    // ignore value for now?

    SMGRegionObject(){}

    SMGRegionObject(SMGRegionObject& o, int new_id){
        id = new_id;
        label = o.label;
        size_low = o.size_low;
        size_high = o.size_high;
    }
};

inline void from_json(const json& j, SMGRegionObject& o) {
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

    SMGEmptyObject(SMGEmptyObject& o, int new_id){
        id = new_id;
        label = o.label;
        size = o.size;
        placement = o.placement;
    }
};

inline void from_json(const json& j, SMGEmptyObject& o) {
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

class SMGValue{
  public:
    int id;
    std::string label;
    int obj{0};
    int offset_low{0};
    int offset_high{0};
    bool lonely{false};
    std::string value;
    // Don't care about lonely, value, iid?

    std::string memory_location;
    std::string target_spec_label;
    int size_high;
    int size_low;
    bool is_null{false};
    bool is_null_with_offset{false};
    bool is_unknown{false};
    bool is_var{false};
    SMGVarObject var;
    bool is_mem{false};
    std::string mem_loc_file;
    int mem_loc_line;
    int mem_loc_column;
    llvm::Value* llvm_val{nullptr};

    SMGValue(){}

    SMGValue(int _id, std::string _label){
        id = _id;
        label = _label;
    }

    bool operator==(const SMGValue& other) const {return (id == other.id);}
};

inline void from_json(const json& j, SMGValue& v) {
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
    if (j.find("targetSpecLabel") != j.end()){
        j.at("targetSpecLabel").get_to(v.target_spec_label);
    }

    if (j.find("value") != j.end() && j.at("value").is_string()){
        j.at("value").get_to(v.value);
    }

    v.is_null = false;
    v.is_null_with_offset = false;
    v.is_var = false;
    v.is_mem = false;
    v.llvm_val = nullptr;

    if (j.find("loc") != j.end()){
        json l = j.at("loc");
        l.at("file").get_to(v.mem_loc_file);
        l.at("line").get_to(v.mem_loc_line);
        l.at("column").get_to(v.mem_loc_column);
        //l.at("insn").get_to(v.memLocInsn);
        v.is_mem = true;
    }
    if (v.label == "NULL"){
        v.is_null = true;
    } else if (v.label == "VO_UNKNOWN" || v.label == "VO_ASSIGNED"){
        v.is_unknown = true;
    }
}

class SMGEdge{
  public:
    int from;
    int to;
    std::string label;
    int off{0};

    SMGEdge(){}

    SMGEdge(int _from, int _to, int _off){
        from = _from;
        to = _to;
        off = _off;
    }

    SMGEdge(int _from, int _to, std::string _label, int _off){
        from = _from;
        to = _to;
        label = _label;
        off = _off;
    }

    bool operator==(const SMGEdge& other) const {return (from == other.from && to == other.to);}
};

inline void from_json(const json& j, SMGEdge& e) {
    j.at("from").get_to(e.from);
    j.at("to").get_to(e.to);
    j.at("label").get_to(e.label);
    if (j.find("off") != j.end()){
        j.at("off").get_to(e.off);
    }
}

class SMGRegionCompositeObject : public SMGGenericObject{
  public:
    std::vector<SMGSimpleObjectTypeVariant> objects;

    SMGRegionCompositeObject(){}
};

inline void from_json(const json& j, SMGRegionCompositeObject& c) {
    j.at("id").get_to(c.id);
    j.at("label").get_to(c.label);
    for (auto o : j["objects"]){
        if (o["label"] == "empty" || o["label"] == "UNIFORM_BLOCK"){
            c.objects.push_back(o.template get<SMGEmptyObject>());
        } else if (o["label"] == "SC_ON_STACK" || o["label"] == "SC_ON_HEAP"){
            c.objects.push_back(o.template get<SMGRegionObject>());
        } else if (o["label"] == "SC_STATIC"){
            c.objects.push_back(o.template get<SMGVarObject>());
        } else {
            llvm::errs() << "unknown object label: " << o["label"].template get<std::string>() << "\n";
        }
    }
}

class SMGLinkedListCompositeObject : public SMGGenericObject{
  public:
    std::vector<SMGSimpleObjectTypeVariant> objects;
    int obj_id;
    int head_offset;
    int prev_offset;
    int next_offset;
    int seg_min_len;

    SMGLinkedListCompositeObject(){}

    SMGLinkedListCompositeObject(SMGLinkedListCompositeObject& old, int new_id){
        id = new_id;
        label = old.label;
        obj_id = old.obj_id;
        head_offset = old.head_offset;
        prev_offset = old.prev_offset;
        next_offset = old.next_offset;
        seg_min_len = old.seg_min_len;
    }

    SMGValue* getFirstValue(std::vector<SMGValue> &values){
        // Try to find the value without any offsets first
        for(auto &value : values){
            if (value.obj == obj_id && value.target_spec_label == "TS_FIRST" && value.offset_low == 0){
                return &value;
            }
        }
        for(auto &value : values){
            if (value.obj == obj_id && value.target_spec_label == "TS_FIRST"){
                return &value;
            }
        }
        return nullptr;
    }

    SMGValue* getLastValue(std::vector<SMGValue> &values){
        // Try to find the value without any offsets first
        for(auto &value : values){
            if (value.obj == obj_id && value.target_spec_label == "TS_LAST" && value.offset_low == 0){
                return &value;
            }
        }
        for(auto &value : values){
            if (value.obj == obj_id && value.target_spec_label == "TS_LAST"){
                return &value;
            }
        }
        return nullptr;
    }

    int getNextObjectId(std::vector<SMGEdge> edges){
        int target_id = obj_id;
        int target_offset = next_offset;
        SMGEdge &nextPointer = *std::find_if(edges.begin(), edges.end(), [&target_id, &target_offset](SMGEdge e){return e.from == target_id && e.off == target_offset;});
        return nextPointer.to;
    }

    int getPrevObjectId(std::vector<SMGEdge> edges){
        int target_id = obj_id;
        int target_offset = prev_offset;
        SMGEdge &prevPointer = *std::find_if(edges.begin(), edges.end(), [&target_id, &target_offset](SMGEdge e){return e.from == target_id && e.off == target_offset;});
        return prevPointer.to;
    }

    int getObjectIdByOffset(std::vector<SMGEdge> edges, int target_offset){
        int target_id = obj_id;
        SMGEdge &objPointer = *std::find_if(edges.begin(), edges.end(), [&target_id, &target_offset](SMGEdge e){return e.from == target_id && e.off == target_offset;});
        return objPointer.to;
    }
};

typedef std::variant<SMGGenericObject, SMGVarObject, SMGRegionObject, SMGEmptyObject, SMGRegionCompositeObject, SMGLinkedListCompositeObject> SMGObjectTypeVariant;

inline void from_json(const json& j, SMGLinkedListCompositeObject& c) {
    j.at("id").get_to(c.id);
    j.at("label").get_to(c.label);
    for (auto o : j["objects"]){
        if (o["label"] == "empty"){
            c.objects.push_back(o.template get<SMGEmptyObject>());
        } else if (o["label"] == "SC_ON_STACK" || o["label"] == "SC_ON_HEAP"){
            SMGRegionObject obj = o.template get<SMGRegionObject>();
            c.obj_id = obj.id;
            c.objects.push_back(obj);
        } else {
            llvm::errs() << "unknown object label: " << o["label"].template get<std::string>() << "\n";
        }
    }
    j.at("headOffset").get_to(c.head_offset);
    j.at("nextOffset").get_to(c.next_offset);
    if (j.find("prevOffset") != j.end()){
        j.at("prevOffset").get_to(c.prev_offset);
    }
    j.at("segMinLength").get_to(c.seg_min_len);
}

inline int objectVariantGetId(SMGSimpleObjectTypeVariant obj){
    // TODO simplify this using templates or something
    if (std::holds_alternative<SMGGenericObject>(obj)){
        return std::get<SMGGenericObject>(obj).id;
    } else if (std::holds_alternative<SMGVarObject>(obj)){
        return std::get<SMGVarObject>(obj).id;
    } else if (std::holds_alternative<SMGRegionObject>(obj)){
        return std::get<SMGRegionObject>(obj).id;
    } else if (std::holds_alternative<SMGEmptyObject>(obj)){
        return std::get<SMGEmptyObject>(obj).id;
    } else {
        llvm::errs() << "Unknown object found in (simple)objectVariantGetId" << "\n";
        return -1;
    }
}

inline int objectVariantGetId(SMGObjectTypeVariant obj){
    // TODO simplify this using templates or something
    if (std::holds_alternative<SMGGenericObject>(obj)){
        return std::get<SMGGenericObject>(obj).id;
    } else if (std::holds_alternative<SMGVarObject>(obj)){
        return std::get<SMGVarObject>(obj).id;
    } else if (std::holds_alternative<SMGRegionObject>(obj)){
        return std::get<SMGRegionObject>(obj).id;
    } else if (std::holds_alternative<SMGEmptyObject>(obj)){
        return std::get<SMGEmptyObject>(obj).id;
    } else if (std::holds_alternative<SMGRegionCompositeObject>(obj)){
        return std::get<SMGRegionCompositeObject>(obj).id;
    } else if (std::holds_alternative<SMGLinkedListCompositeObject>(obj)){
        return std::get<SMGLinkedListCompositeObject>(obj).id;
    } else {
        llvm::errs() << "Unknown object found in objectVariantGetId" << "\n";
        return -1;
    }
}

inline std::vector<SMGSimpleObjectTypeVariant>::iterator getObjectById(int id, std::vector<SMGSimpleObjectTypeVariant> &objects){
    // TODO simplify this using templates or something
    std::vector<SMGSimpleObjectTypeVariant>::iterator it = objects.begin();
    while (it != objects.end()){
        if (objectVariantGetId(*it) == id){
            break;
        }
        ++it;
    }
    return it;
}

inline std::vector<SMGObjectTypeVariant>::iterator getObjectById(int id, std::vector<SMGObjectTypeVariant> &objects){
    // TODO simplify this using templates or something
    std::vector<SMGObjectTypeVariant>::iterator it = objects.begin();
    while (it != objects.end()){
        if (objectVariantGetId(*it) == id){
            break;
        }
        ++it;
    }
    return it;
}

inline std::vector<SMGValue>::iterator getValueById(int id, std::vector<SMGValue> &values){
    std::vector<SMGValue>::iterator it = values.begin();
    while (it != values.end()){
        if (it->id == id){
            break;
        }
        ++it;
    }
    return it;
}

inline std::optional<SMGObjectTypeVariant> convertSMGObjectVariant(SMGSimpleObjectTypeVariant obj){
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

inline std::pair<std::vector<SMGEdge*>, std::vector<SMGEdge*>> getEdgesWithId(int id, std::vector<SMGEdge> &edges){
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

inline int findUniqueId(std::vector<SMGValue> &values, std::vector<SMGObjectTypeVariant> &objects, std::vector<SMGEdge> &edges){
    static int i = 0;
    for(i++; true; i++){
        if(getObjectById(i, objects) != objects.end()){
            continue;
        }
        if(getValueById(i, values) != values.end()){
            continue;
        }
        auto found_edges = getEdgesWithId(i, edges);
        if(!found_edges.first.empty() || !found_edges.second.empty()){
            continue;
        }
        return i;
    }
}

}

#endif // DG_SMG_OBJECTS_H_
