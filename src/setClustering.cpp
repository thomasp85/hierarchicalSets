#include <Rcpp.h>
#include <iostream>
#include <fstream>

using namespace Rcpp;

struct Cluster {
    std::vector<int> members;
    std::string label;
    bool leaf;
    double homogeneity;
    int iSize;
    int uSize;
    int id1;
    int id2;
};
bool homogeneityComp(const Cluster& a, const Cluster& b) {
    if (a.homogeneity == b.homogeneity) {
        return a.uSize > b.uSize;
    } else {
        return a.homogeneity < b.homogeneity;
    }
}

std::pair<int, int> setHomology(IntegerVector& P, IntegerVector& I, std::vector<int> sets, double threshold) {
    int i;
    int maxElem = 0;
    int nSets = sets.size();

    for (i = 0; i < nSets; ++i) {
        maxElem += P[sets[i]+1] - P[sets[i]];
    }
    std::vector<int> elements;
    elements.reserve(maxElem);
    IntegerVector::iterator iStart = I.begin();
    for (i = 0; i < nSets; ++i) {
        std::copy(iStart + P[sets[i]], iStart + P[sets[i]+1], std::back_inserter(elements));
    }
    maxElem = elements.size();
    if (maxElem == 0) {
        return std::pair<int, int>(0, 0);
    }
    std::sort(elements.begin(), elements.end());
    int iSize = 0;
    int uSize = 0;
    int currentElem = elements[0];
    int currentCount = 1;
    for (i = 0; i <= maxElem; ++i) {
        if (i != maxElem && elements[i] == currentElem) {
            ++currentCount;
        } else {
            ++uSize;
            if (double(currentCount)/nSets >= threshold) {
                ++iSize;
            }
            if (i < maxElem) {
                currentElem = elements[i];
                currentCount = 1;
            }
        }
    }
    return std::pair<int, int>(iSize, uSize);
}

std::vector<int> getIntersection(IntegerVector& I, IntegerVector& P, std::vector<int> sets) {
    int i;
    int maxElem = 0;
    int nSets = sets.size();

    for (i = 0; i < nSets; ++i) {
        maxElem = std::max(maxElem, P[sets[i]+1] - P[sets[i]]);
    }
    int *intersect = new int[maxElem];
    int *intersect2 = new int[maxElem];
    int *intersectTmp;
    int *intersectEnd;
    IntegerVector::iterator setStart = I.begin();

    intersectEnd = std::copy(setStart + P[sets[0]], setStart + P[sets[0]+1], intersect);
    for (i = 1; i < nSets; ++i) {
        intersectEnd = std::set_intersection(intersect, intersectEnd, setStart + P[sets[i]], setStart + P[sets[i]+1], intersect2);
        intersectTmp = intersect;
        intersect = intersect2;
        intersect2 = intersectTmp;
    }
    std::vector<int> results(intersect, intersectEnd);

    delete[] intersect;
    delete[] intersect2;

    return results;
}

List createTree(std::map<int, Cluster>& nodes, int key, CharacterVector& setNames) {
    std::map<int, Cluster>::iterator it;
    std::string label;
    List tree;
    int x1, x2;
    double members;
    it = nodes.find(key);
    if (it == nodes.end()) {
        stop("Missing key in nodes");
    }
    if (it->second.leaf){
        tree = List::create(wrap(it->second.members[0] + 1));
        label = setNames[it->second.members[0]];
        tree.attr("label") = label;
    } else {
        tree = List::create(
            createTree(nodes, it->second.id1, setNames),
            createTree(nodes, it->second.id2, setNames)
        );
        x1 = as<List>(tree[0]).attr("leaf") ? 0 : as<List>(tree[0]).attr("midpoint");
        x2 = as<List>(tree[1]).attr("leaf") ? 0 : as<List>(tree[1]).attr("midpoint");
        members = as<List>(tree[0]).attr("members");
        tree.attr("midpoint") = (members + x1 + x2) / 2;
    }
    tree.attr("members") = it->second.members.size();
    tree.attr("height") = (1.0 / it->second.homogeneity) - 1;
    tree.attr("leaf") = it->second.leaf;
    tree.attr("intersect") = it->second.iSize;
    tree.attr("union") = it->second.uSize;
    IntegerVector membersets(it->second.members.begin(), it->second.members.end());
    tree.attr("memberSets") = membersets + 1;
    tree.attr("class") = "dendrogram";
    return tree;
}

std::vector<int> getOutliersRecurse(List clusters, std::deque<int>& from, std::deque<int>& to, std::deque<IntegerVector>& elements, IntegerVector& P, IntegerVector& I, bool count) {
    bool leaf = clusters.attr("leaf");
    if (!leaf) {
        int i, j;
        std::vector<int> pair(2);
        IntegerVector sets1 = as<List>(clusters[0]).attr("memberSets");
        IntegerVector sets2 = as<List>(clusters[1]).attr("memberSets");
        std::vector<int> nodeIntersect;
        std::vector<int> intersect1 = getOutliersRecurse(as<List>(clusters[0]), from, to, elements, P, I, count);
        std::vector<int> intersect2 = getOutliersRecurse(as<List>(clusters[1]), from, to, elements, P, I, count);
        nodeIntersect.reserve(std::max(intersect1.size(), intersect2.size()));
        std::set_intersection(intersect1.begin(), intersect1.end(), intersect2.begin(), intersect2.end(), std::back_inserter(nodeIntersect));
        std::vector<int> pairIntersect;
        std::vector<int> outliers;
        for (i = 0; i < sets1.size(); ++i) {
            pair[0] = sets1[i] - 1;
            for (j = 0; j < sets2.size(); ++j) {
                pair[1] = sets2[j] - 1;
                pairIntersect = getIntersection(I, P, pair);
                outliers.clear();
                outliers.reserve(pairIntersect.size());
                std::set_difference(pairIntersect.begin(), pairIntersect.end(), nodeIntersect.begin(), nodeIntersect.end(), std::back_inserter(outliers));
                if (outliers.size() != 0) {
                    from.push_back(std::min(sets1[i], sets2[j]));
                    to.push_back(std::max(sets1[i], sets2[j]));
                    if (count) {
                        elements.push_back(IntegerVector(1, outliers.size()));
                    } else {
                        elements.push_back(IntegerVector(outliers.begin(), outliers.end()) + 1);
                    }
                }
            }
        }
        return nodeIntersect;
    } else {
        std::vector<int> sets(1, as<IntegerVector>(clusters.attr("memberSets"))[0] - 1);
        return getIntersection(I, P, sets);
    }
}

void getBetweenClusterOutliers(IntegerVector& sets1, IntegerVector& sets2, std::deque<int>& from, std::deque<int>& to, std::deque<IntegerVector>& elements, IntegerVector& P, IntegerVector& I, bool count) {
    int i, j;
    std::vector<int> pair(2);
    std::vector<int> intersection;
    for (i = 0; i < sets1.size(); ++i) {
        pair[0] = sets1[i] - 1;
        for (j = 0; j < sets2.size(); ++j) {
            pair[1] = sets2[j] - 1;
            intersection = getIntersection(I, P, pair);
            if (intersection.size() != 0) {
                from.push_back(std::min(sets1[i], sets2[j]));
                to.push_back(std::max(sets1[i], sets2[j]));
                if (count) {
                    elements.push_back(IntegerVector(1, intersection.size()));
                } else {
                    elements.push_back(IntegerVector(intersection.begin(), intersection.end()) + 1);
                }
            }
        }
    }
}

//[[Rcpp::export]]
List setClustering(IntegerVector P, IntegerVector I, CharacterVector setNames, double threshold) {
    int nSets = P.size() - 1;
    int idCounter = 0;
    int i, size;
    std::pair<int, int> homology;
    std::vector<int> sets;
    sets.reserve(nSets);

    std::map<int, Cluster> allNodes;
    std::map<int, Cluster>::iterator familyIt1;
    std::map<int, Cluster>::iterator familyIt2;
    for (i = 0; i < nSets; ++i) {
        size = P[i + 1] - P[i];
        Cluster cluster;
        cluster.members = std::vector<int>(1, i);
        cluster.leaf = true;
        cluster.iSize = size;
        cluster.uSize = size;
        cluster.homogeneity = 1.0;
        allNodes[idCounter++] = cluster;
    }
    std::set<int> currentFamilies;
    std::set<int>::iterator idIt;
    for (familyIt1 = allNodes.begin(); familyIt1 != allNodes.end(); ++familyIt1) {
        currentFamilies.insert(familyIt1->first);
    }
    std::deque<Cluster> possibles;
    for (familyIt1 = allNodes.begin(); familyIt1 != allNodes.end(); ++familyIt1) {
        familyIt2 = familyIt1;
        ++familyIt2;
        for (; familyIt2 != allNodes.end(); ++familyIt2) {
            sets.clear();
            sets.insert(sets.end(), familyIt1->second.members.begin(), familyIt1->second.members.end());
            sets.insert(sets.end(), familyIt2->second.members.begin(), familyIt2->second.members.end());
            homology = setHomology(P, I, sets, threshold);
            if (homology.first != 0 && homology.second != 0) {
                Cluster family;
                family.id1 = familyIt1->first;
                family.id2 = familyIt2->first;
                family.members = sets;
                family.iSize = homology.first;
                family.uSize = homology.second;
                family.homogeneity = double(homology.first) / homology.second;
                family.leaf = false;
                possibles.push_back(family);
            }
        }
    }
    std::sort(possibles.begin(), possibles.end(), homogeneityComp);
    while (!possibles.empty()) {
        // Get next to merge
        Cluster nextFamily = possibles.back();
        possibles.pop_back();
        idIt = currentFamilies.find(nextFamily.id1);
        currentFamilies.erase(idIt);
        idIt = currentFamilies.find(nextFamily.id2);
        currentFamilies.erase(idIt);
        // Calculate all possible merges with the next merge
        for (idIt = currentFamilies.begin(); idIt != currentFamilies.end(); ++idIt) {
            familyIt1 = allNodes.find(*idIt);
            sets.clear();
            sets.insert(sets.end(), nextFamily.members.begin(), nextFamily.members.end());
            sets.insert(sets.end(), familyIt1->second.members.begin(), familyIt1->second.members.end());
            homology = setHomology(P, I, sets, threshold);
            if (homology.first != 0 && homology.second != 0) {
                Cluster family;
                family.id1 = idCounter;
                family.id2 = familyIt1->first;
                family.members = sets;
                family.iSize = homology.first;
                family.uSize = homology.second;
                family.homogeneity = (double)homology.first / (double)homology.second;
                family.leaf = false;
                possibles.push_back(family);
            }
        }

        // Add next merge
        currentFamilies.insert(idCounter);
        allNodes[idCounter++] = nextFamily;

        // Prune and sort possibles
        std::sort(possibles.begin(), possibles.end(), homogeneityComp);
        for (i = possibles.size() - 1; i >= 0; --i) {
            if (currentFamilies.find(possibles[i].id1) == currentFamilies.end()) {
                possibles.pop_back();
            } else if (currentFamilies.find(possibles[i].id2) == currentFamilies.end()) {
                possibles.pop_back();
            } else {
                break;
            }
        }
    }
    std::vector<List> clusters;
    for (idIt = currentFamilies.begin(); idIt != currentFamilies.end(); ++ idIt) {
        clusters.push_back(createTree(allNodes, *idIt, setNames));
    }
    return wrap(clusters);
}

//[[Rcpp::export]]
DataFrame pairSummary(IntegerVector P, IntegerVector I, IntegerVector order) {
    int nSets = order.size();
    std::deque<int> x;
    std::deque<int> y;
    std::deque<int> iSize;
    std::deque<int> uSize;
    int i, j;
    std::pair<int, int> homology;
    std::vector<int> sets(2);
    for (i = 0; i < (nSets - 1); ++i) {
        for(j = i + 1; j < nSets; ++j) {
            x.push_back(i+1);
            y.push_back(j+1);
            sets[0] = order[i] - 1;
            sets[1] = order[j] - 1;
            homology = setHomology(P, I, sets, 1);
            iSize.push_back(homology.first);
            uSize.push_back(homology.second);
        }
    }
    return DataFrame::create(
        Named("x") = wrap(x),
        Named("y") = wrap(y),
        Named("intersect") = wrap(iSize),
        Named("union") = wrap(uSize)
    );
}

//[[Rcpp::export]]
List getOutliers(List clusters, IntegerVector P, IntegerVector I, bool count) {
    int i, j;
    IntegerVector members1, members2;
    std::deque<int> from;
    std::deque<int> to;
    std::deque<IntegerVector> outliers;

    for (i = 0; i < clusters.size(); ++i) {
        getOutliersRecurse(as<List>(clusters[i]), from, to, outliers, P, I, count);

        members1 = as<List>(clusters[i]).attr("memberSets");
        for (j = i + 1; j < clusters.size(); ++j) {
            members2 = as<List>(clusters[j]).attr("memberSets");
            getBetweenClusterOutliers(members1, members2, from, to, outliers, P, I, count);
        }
    }

    return List::create(
        Named("from") = wrap(from),
        Named("to") = wrap(to),
        Named("outliers") = wrap(outliers)
    );
}
