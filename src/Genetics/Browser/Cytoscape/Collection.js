"use strict";

exports.collectionJson = function(coll) {
    return coll.jsons();
};

exports.collectionsEqual = function(collA, collB) {
    return collA.same(collB);
};

exports.union = function(a, b) {
    return a.union(b);
};

exports.size = function(coll) {
    return coll.size();
};

exports.containsImpl = function(collA, collB) {
    return collA.contains(collB);
};

exports.emptyCollection = function(cy) {
    return require("cytoscape")().collection();
};

exports.connectedEdges = function(a) {
    return a.connectedEdges();
};

exports.connectedNodes = function(a) {
    return a.connectedNodes();
};

exports.sourceNodes = function(a) {
    return a.sources();
};

exports.targetNodes = function(a) {
    return a.targets();
};

exports.filterImpl = function(pred, coll) {
    return coll.filter(pred);
};

exports.isNode = function(elem) {
    return elem.isNode();
};

exports.isEdge = function(elem) {
    return elem.isEdge();
};
