"use strict";

exports.collectionJson = function(coll) {
    return coll.jsons();
};

exports.collectionsEqual = function(collA) {
    return function(collB) {
        return collA.same(collB);
    };
};

exports.union = function(a) {
    return function(b) {
        return a.union(b);
    };
};

exports.size = function(coll) {
    return coll.size();
};

exports.contains = function(collA) {
    return function(collB) {
        return collA.contains(collB);
    };
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

exports.filter = function(pred) {
    return function(coll) {
        return coll.filter(pred);
    };
};

exports.isNode = function(elem) {
    return elem.isNode();
};

exports.isEdge = function(elem) {
    return elem.isEdge();
};


exports.collectionsEqual = function(collA) {
    return function(collB) {
        return collA.same(collB);
    };
};
