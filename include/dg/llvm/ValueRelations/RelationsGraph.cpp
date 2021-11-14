#include "RelationsGraph.h"

#include <algorithm>
#include <cmath>
#include <iterator>

namespace dg {
namespace vr {

RelationType toRelation(size_t i) { return static_cast<RelationType>(i); }

size_t toInt(RelationType t) { return static_cast<size_t>(t); }

size_t p(RelationType t) { return pow(2, static_cast<size_t>(t)); }

RelationType inverted(RelationType type) {
    switch (type) {
    case RelationType::EQ:
        return RelationType::EQ;
    case RelationType::NE:
        return RelationType::NE;
    case RelationType::LE:
        return RelationType::GE;
    case RelationType::LT:
        return RelationType::GT;
    case RelationType::GE:
        return RelationType::LE;
    case RelationType::GT:
        return RelationType::LT;
    case RelationType::PT:
        return RelationType::PF;
    case RelationType::PF:
        return RelationType::PT;
    }
    assert(0 && "unreachable");
    abort();
}

RelationType negated(RelationType type) {
    switch (type) {
    case RelationType::EQ:
        return RelationType::NE;
    case RelationType::NE:
        return RelationType::EQ;
    case RelationType::LE:
        return RelationType::GT;
    case RelationType::LT:
        return RelationType::GE;
    case RelationType::GE:
        return RelationType::LT;
    case RelationType::GT:
        return RelationType::LE;
    default:
        assert(0 && "no negation for relation");
        abort();
    }
}

RelationBits conflicting(RelationType type) {
    switch (type) {
    case RelationType::EQ:
        return p(RelationType::NE) & p(RelationType::LT) & p(RelationType::GT);
    case RelationType::NE:
        return p(RelationType::EQ);
    case RelationType::LT:
        return p(RelationType::EQ);
    case RelationType::GT:
        return p(RelationType::EQ);
    case RelationType::LE:
    case RelationType::GE:
    case RelationType::PT:
    case RelationType::PF:
        return 0;
    }
    assert(0 && "unreachable");
    abort();
}

void addImplied(RelationBits &bits) {
    for (size_t i = 0; i < allRelations.size(); ++i) {
        if (bits[i]) {
            switch (toRelation(i)) {
            case RelationType::EQ:
                bits.set(toInt(RelationType::LE));
                bits.set(toInt(RelationType::GE));
                break;
            case RelationType::LT:
                bits.set(toInt(RelationType::LE));
                bits.set(toInt(RelationType::NE));
                break;
            case RelationType::GT:
                bits.set(toInt(RelationType::GE));
                bits.set(toInt(RelationType::NE));
                break;
            case RelationType::NE:
            case RelationType::LE:
            case RelationType::GE:
            case RelationType::PT:
            case RelationType::PF:
                break;
            }
        }
    }
}

bool transitive(RelationType type) {
    switch (type) {
    case RelationType::EQ:
    case RelationType::LE:
    case RelationType::LT:
    case RelationType::GE:
    case RelationType::GT:
        return true;
    case RelationType::NE:
    case RelationType::PT:
    case RelationType::PF:
        return false;
    }
    assert(0 && "unreachable");
    abort();
}

bool transitiveOver(RelationType fst, RelationType snd) {
    switch (fst) {
    case RelationType::LE:
    case RelationType::LT:
        return snd == RelationType::LE || snd == RelationType::LT;
    case RelationType::GE:
    case RelationType::GT:
        return snd == RelationType::GE || snd == RelationType::GT;
    case RelationType::EQ:
    case RelationType::NE:
    case RelationType::PT:
    case RelationType::PF:
        // assert(0 && "unreachable");
        return false;
    }
    assert(0 && "unreachable");
    abort();
}

const RelationBits allRelations = ~0;

#ifndef NDEBUG
std::ostream &operator<<(std::ostream &out, RelationType r) {
    switch (r) {
    case RelationType::EQ:
        out << "EQ";
        break;
    case RelationType::NE:
        out << "NE";
        break;
    case RelationType::LE:
        out << "LE";
        break;
    case RelationType::LT:
        out << "LT";
        break;
    case RelationType::GE:
        out << "GE";
        break;
    case RelationType::GT:
        out << "GT";
        break;
    case RelationType::PT:
        out << "POINTS_TO";
        break;
    case RelationType::PF:
        out << "POINTED_FROM";
        break;
    }
    return out;
}
#endif

using RelationsMap = RelationsGraph::RelationsMap;

bool strict(RelationType type) {
    return type == RelationType::LT || type == RelationType::GT;
}

RelationType getNonStrict(RelationType type) {
    switch (type) {
    case RelationType::LT:
        return RelationType::LE;
    case RelationType::GT:
        return RelationType::GE;
    default:
        assert(0 && "no nonstrict variant");
        abort();
    }
}

RelationBits getAugmented(const RelationBits &relations) {
    RelationBits augmented = relations;
    for (RelationType type : {RelationType::LT, RelationType::GT}) {
        augmented[toInt(type)] = augmented[toInt(getNonStrict(type))] =
                augmented[toInt(type)] || augmented[toInt(getNonStrict(type))];
    }
    return augmented;
}

RelationsMap filterResult(const RelationBits &relations,
                          const RelationsMap &result) {
    RelationsMap filtered;
    std::copy_if(result.begin(), result.end(),
                 std::inserter(filtered, filtered.begin()),
                 [&relations](const auto &pair) {
                     return (relations & pair.second).any();
                 });
    return filtered;
}

bool shouldAdd(Bucket::RelationEdge &edge, Bucket &start) {
    return edge.from() == start || transitive(edge.rel());
}

bool shouldSkip(const Bucket::RelationEdge &edge) {
    return !transitive(edge.rel());
}

RelationsMap getAugmentedRelated(const RelationsGraph &graph, Bucket &start,
                                 const RelationBits &relations,
                                 bool toFirstStrict) {
    RelationsMap result;
    result[start].set(toInt(RelationType::EQ));

    Bucket::BucketSet nestedVisited;
    for (auto it = graph.begin_related(start, relations);
         it != graph.end_related(start); ++it) {
        result[it->to()].set(toInt(it->rel()));

        if (!strict(it->rel()))
            continue;

        result[it->to()].set(toInt(getNonStrict(it->rel())), false);

        for (auto nestedIt =
                     it->to().begin(nestedVisited, relations, true, true);
             nestedIt != it->to().end(nestedVisited); ++nestedIt) {
            if (toFirstStrict)
                result[nestedIt->to()].set(toInt(it->rel()), false);
            else
                result[nestedIt->to()].set(toInt(it->rel()));
        }
    }
    return result;
}

RelationsMap RelationsGraph::getRelated(Bucket &start,
                                        const RelationBits &relations,
                                        bool toFirstStrict) const {
    RelationBits augmented = getAugmented(relations);

    RelationsMap result =
            getAugmentedRelated(*this, start, augmented, toFirstStrict);

    for (auto &pair : result) {
        addImplied(pair.second);
    }

    return relations != augmented ? filterResult(relations, result)
                                  : std::move(result);
}

RelationBits fromMaybeBetween(const RelationsGraph &graph, Bucket &lt,
                              Bucket &rt, RelationBits *maybeBetween) {
    return maybeBetween ? *maybeBetween : graph.relationsBetween(lt, rt);
}

bool RelationsGraph::areRelated(Bucket &lt, RelationType type, Bucket &rt,
                                RelationBits *maybeBetween) const {
    RelationBits between = fromMaybeBetween(*this, lt, rt, maybeBetween);
    return between[toInt(type)];
}

bool RelationsGraph::haveConflictingRelation(Bucket &lt, RelationType type,
                                             Bucket &rt,
                                             RelationBits *maybeBetween) const {
    switch (type) {
    case RelationType::EQ:
    case RelationType::NE:
    case RelationType::LT:
    case RelationType::LE:
    case RelationType::GT:
    case RelationType::GE: {
        RelationBits between = fromMaybeBetween(*this, lt, rt, maybeBetween);
        return (between & conflicting(type)).any();
    }
    case RelationType::PT:
        return lt.hasRelation(type) &&
               haveConflictingRelation(lt.getRelated(type), RelationType::EQ,
                                       rt);
    case RelationType::PF:
        return haveConflictingRelation(rt, inverted(type), lt);
    }
    assert(0 && "unreachable");
    abort();
}

Bucket::BucketSet getIntersectingNonstrict(const RelationsGraph &graph,
                                           Bucket &lt, Bucket &rt) {
    RelationsMap ltGE = graph.getRelated(lt, p(RelationType::GE));
    RelationsMap rtLE = graph.getRelated(rt, p(RelationType::LE));

    RelationsMap result;
    std::set_intersection(ltGE.begin(), ltGE.end(), rtLE.begin(), rtLE.end(),
                          std::inserter(result, result.begin()),
                          [](auto &ltPair, auto &rtPair) {
                              return ltPair.first == rtPair.first;
                          });

    Bucket::BucketSet other;
    for (auto &pari : result)
        other.emplace(pari.first);

    return other;
}

RelationBits inverted(const RelationBits &other) {
    RelationBits newBits;
    for (size_t i = 0; i < allRelations.size(); ++i) {
        newBits.set(toInt(inverted(toRelation(i))), other[i]);
    }
    return newBits;
}

void RelationsGraph::addRelation(Bucket &lt, RelationType type, Bucket &rt,
                                 RelationBits *maybeBetween) {
    RelationBits between = fromMaybeBetween(*this, lt, rt, maybeBetween);
    if (areRelated(lt, type, rt, &between))
        return;
    assert(!haveConflictingRelation(lt, type, rt, &between));

    switch (type) {
    case RelationType::EQ:
        return setEqual(lt, rt);
    case RelationType::NE:
        if (areRelated(lt, RelationType::LE, rt, &between)) {
            unsetRelated(lt, RelationType::LE, rt);
            return addRelation(lt, RelationType::LT, rt, &between);
        }
        break; // jump after switch
    case RelationType::LT:
        if (areRelated(lt, RelationType::LE, rt, &between))
            unsetRelated(lt, RelationType::LE, rt);
        break; // jump after switch
    case RelationType::LE:
        if (areRelated(lt, RelationType::NE, rt, &between)) {
            unsetRelated(lt, RelationType::NE, rt);
            return addRelation(lt, RelationType::LT, rt, &between);
        }
        if (areRelated(lt, RelationType::GE, rt, &between)) {
            Bucket::BucketSet intersect =
                    getIntersectingNonstrict(*this, lt, rt);
            Bucket &first = *intersect.begin();
            for (auto it = std::next(intersect.begin()); it != intersect.end();
                 ++it)
                setEqual(first, *it);
            return;
        }
        break; // jump after switch
    case RelationType::PT:
        if (lt.hasRelation(type))
            return addRelation(lt.getRelated(type), RelationType::EQ, rt);
        break; // jump after switch
    case RelationType::GT:
    case RelationType::GE:
    case RelationType::PF: {
        RelationBits betweenInverted = inverted(between);
        return addRelation(rt, inverted(type), lt, &betweenInverted);
    }
        return setRelated(lt, RelationType::LE, rt);
    case RelationType::PT:
        if (lt.hasRelation(type))
            return addRelation(*(lt.relatedBuckets.at(type).begin()),
                               RelationType::EQ, rt);
        return setRelated(lt, type, rt);
    case RelationType::GT:
    case RelationType::GE:
    case RelationType::PF: {
        RelationBits betweenInverted = inverted(between);
        return addRelation(rt, inverted(type), lt, &betweenInverted);
    }
        return setRelated(lt, RelationType::NE, rt);
    case RelationType::LT:
        if (areRelated(lt, RelationType::LE, rt, &between))
            unsetRelated(lt, RelationType::LE, rt);
        return setRelated(lt, RelationType::LE, rt);
    case RelationType::LE:
        if (areRelated(lt, RelationType::NE, rt, &between)) {
            unsetRelated(lt, RelationType::NE, rt);
            return addRelation(lt, RelationType::LT, rt, &between);
        }
        if (areRelated(lt, RelationType::GE, rt, &between)) {
            BucketSet intersect = getIntersectingNonstrict(*this, lt, rt);
            Bucket &first = *intersect.begin();
            for (auto it = std::next(intersect.begin()); it != intersect.end();
                 ++it)
                setEqual(first, *it);
            return;
        }
        return setRelated(lt, RelationType::LE, rt);
    case RelationType::PT:
        if (lt.hasRelation(type))
            return addRelation(*(lt.relatedBuckets.at(type).begin()),
                               RelationType::EQ, rt);
        return setRelated(lt, type, rt);
    case RelationType::GT:
    case RelationType::GE:
    case RelationType::PF: {
        RelationBits betweenInverted = inverted(between);
        return addRelation(rt, inverted(type), lt, &betweenInverted);
    }
    }
    setRelated(lt, type, rt);
}
}

} // namespace vr
} // namespace dg