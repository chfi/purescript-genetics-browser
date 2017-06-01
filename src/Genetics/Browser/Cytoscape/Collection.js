"use strict";

exports.emptyCollection = function(cy) {
    return cy.collection();
exports.collectionJson = function(coll) {
    return coll.jsons();
};
};

exports.union = function(a) {
    return function(b) {
        return a.union(b);
    };
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
