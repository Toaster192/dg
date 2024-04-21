#include "dg/llvm/PointerAnalysis/SMGObjects.h"

namespace dg {

void merge_flags(SMGPTAFlags *target, const SMGPTAFlags source){
    target->null |= source.null;
    target->nullWithOffset |= source.nullWithOffset;
    target->unknown |= source.unknown;
    target->invalidated |= source.invalidated;
}

void from_json(const json& j, SMGGenericObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);
}

void from_json(const json& j, SMGVarObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);

    json v = j.at("value").at("var");
    if (v.find("name") != v.end()){
        v.at("name").get_to(o.varName);
    }
    v.at("uid").get_to(o.varUID);

    json l = j.at("value").at("loc");
    l.at("file").get_to(o.varLocFile);
    l.at("line").get_to(o.varLocLine);
    l.at("column").get_to(o.varLocColumn);
    if (l.find("insn") != l.end()){
        l.at("insn").get_to(o.varLocInsn);
    }

    o.llvmVal = nullptr;
}

void from_json(const json& j, SMGRegionObject& o) {
    j.at("id").get_to(o.id);
    j.at("label").get_to(o.label);

    j.at("size_low").get_to(o.size_low);
    j.at("size_high").get_to(o.size_high);
}

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

    v.is_null = false;
    v.is_var = false;
    v.is_mem = false;
    v.llvmVal = nullptr;

    if (j.find("loc") != j.end()){
        json l = j.at("loc");
        l.at("file").get_to(v.memLocFile);
        l.at("line").get_to(v.memLocLine);
        l.at("column").get_to(v.memLocColumn);
        //l.at("insn").get_to(v.memLocInsn);
        v.is_mem = true;
    }
    if (v.label == "NULL"){
        v.is_null = true;
    }
}

void from_json(const json& j, SMGEdge& e) {
    j.at("from").get_to(e.from);
    j.at("to").get_to(e.to);
    j.at("label").get_to(e.label);
    if (j.find("off") != j.end()){
        j.at("off").get_to(e.off);
    }
}

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

}
