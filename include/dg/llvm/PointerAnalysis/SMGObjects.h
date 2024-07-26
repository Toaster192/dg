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

enum SMGObjectType {VAR, EMPTY, REGION, COMPOSITEREGION, LINKEDLIST};

inline const char* ToString(SMGObjectType t)
{
    switch (t)
    {
        case VAR:             return "Var";
        case EMPTY:           return "Empty";
        case REGION:          return "Region";
        case COMPOSITEREGION: return "CompositeRegion";
        case LINKEDLIST:      return "LinkedList";
        default:              return "[Unknown SMGObjectType]";
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

class SMGObject{
  public:
    /* Generic attributes */
    int id;
    std::string label;
    SMGObjectType type;
    /* Var type attributes */
    std::string var_name{""};
    /* Unused 
    int varUID;
    */
    std::string var_loc_file;
    int var_loc_line;
    int var_loc_column;
    llvm::Value* llvm_val{nullptr};
    /* Region type attributes */
    /* Unused 
    int size_high;
    int size_low;
    */
    /* Empty type attributes */
    /* Unused 
    int size;
    std::string placement;
    */
    /* Composite type attributes */
    std::vector<SMGObject> objects;
    /* Composite linked list type attributes */
    int obj_id;
    int head_offset;
    int prev_offset;
    int next_offset;
    int seg_min_len;

    SMGObject(){}

    static SMGObject copyEmptyObject(SMGObject&, int);
    static SMGObject copyRegionObject(SMGObject&, int);
    static SMGObject copyLinkedList(SMGObject&, int);

    virtual ~SMGObject() = default;

    bool operator==(const SMGObject& other) const {return (id == other.id);}

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

inline SMGObject SMGObject::copyEmptyObject(SMGObject &old, int new_id){
    SMGObject new_empty = SMGObject();
    new_empty.id = new_id;
    new_empty.label = old.label;
    new_empty.type = old.type;
    /* Unused
    new_empty.size = old.size;
    new_empty.placement = old.placement;
    */
    return new_empty;
}

inline SMGObject SMGObject::copyRegionObject(SMGObject &old, int new_id){
    SMGObject new_region = SMGObject();
    new_region.id = new_id;
    new_region.label = old.label;
    new_region.type = old.type;
    /* Unused
    new_region.size_low = old.size_low;
    new_region.size_high = old.size_high;
    */
    return new_region;
}

inline SMGObject SMGObject::copyLinkedList(SMGObject &old, int new_id){
    SMGObject new_list = SMGObject();
    new_list.id = new_id;
    new_list.label = old.label;
    new_list.type = old.type;
    new_list.obj_id = old.obj_id;
    new_list.head_offset = old.head_offset;
    new_list.prev_offset = old.prev_offset;
    new_list.next_offset = old.next_offset;
    new_list.seg_min_len = old.seg_min_len;
    return new_list;
}

inline void from_json(const json& j, SMGObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);

    if(o.label == "field" || o.label == "SC_STATIC"){
        o.type = VAR;
        json v = j.at("value").at("var");
        if (v.find("name") != v.end()){
            v.at("name").get_to(o.var_name);
        }
        /* Unused
        v.at("uid").get_to(o.varUID);
        */

        json l = j.at("value").at("loc");
        l.at("file").get_to(o.var_loc_file);
        l.at("line").get_to(o.var_loc_line);
        l.at("column").get_to(o.var_loc_column);

        o.llvm_val = nullptr;
    } else if (o.label == "SC_ON_STACK" || o.label == "SC_ON_HEAP"){
        o.type = REGION;
        /* Unused
        j.at("size_low").get_to(o.size_low);
        j.at("size_high").get_to(o.size_high);
        */
    } else if (o.label == "empty" || o.label == "UNIFORM_BLOCK"){
        o.type = EMPTY;
        /* Unused
        if (j.find("size") != j.end()){
            j.at("size").get_to(o.size);
        }
        if (j.find("placement") != j.end()){
            j.at("placement").get_to(o.placement);
        }
        */
    } else if (o.label == "region" || o.label == "0..1"){ // TODO: handle 0..1 differently?
        o.type = COMPOSITEREGION;
        for (auto obj : j["objects"]){
            o.objects.push_back(obj.template get<SMGObject>());
        }
    } else if (o.label == "DLS" || o.label == "SLS") {
        o.type = LINKEDLIST;
        for (auto obj : j["objects"]){
            SMGObject new_obj = obj.template get<SMGObject>();
            if (new_obj.type == REGION){
                o.obj_id = new_obj.id;
            }
            o.objects.push_back(new_obj);
        }
        j.at("headOffset").get_to(o.head_offset);
        j.at("nextOffset").get_to(o.next_offset);
        if (j.find("prevOffset") != j.end()){
            j.at("prevOffset").get_to(o.prev_offset);
        }
        j.at("segMinLength").get_to(o.seg_min_len);
    } else {
        llvm::errs() << "unknown object label: " << o.label << "\n";
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
    std::string value;
    // Don't care about lonely, value, iid?

    std::string memory_location;
    std::string target_spec_label;
    /* Unused
    int size_high;
    int size_low;
    */
    bool is_null{false};
    bool is_null_with_offset{false};
    bool is_unknown{false};
    bool is_var{false};
    bool is_mem{false};
    SMGObject var;
    std::string mem_loc_file;
    int mem_loc_line;
    int mem_loc_column;
    llvm::Value* llvm_val{nullptr};

    SMGValue(){}

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
        v.is_mem = true;
    }
    if (v.label == "NULL"){
        v.is_null = true;
    } else if (v.label == "VO_UNKNOWN" || v.label == "VO_ASSIGNED"){
        v.is_unknown = true;
    }
}

inline std::vector<SMGObject>::iterator getObjectById(int id, std::vector<SMGObject> &objects){
    std::vector<SMGObject>::iterator it = objects.begin();
    while (it != objects.end()){
        if (it->id == id){
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

inline int findUniqueId(std::vector<SMGValue> &values, std::vector<SMGObject> &objects, std::vector<SMGEdge> &edges){
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
