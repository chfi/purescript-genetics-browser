"use strict";

exports.cytoscapeImpl = function(htmlElement) {
    return function(eles) {
        return function() {
            var cy = require("cytoscape")({

                container: htmlElement,

                elements: eles,

                style: [ // the stylesheet for the graph TODO: be configurable
                    {
                        selector: 'node',
                        style: {
                            'background-color': '#666',
                            'label': 'data(id)'
                        }
                    },

                    {
                        selector: 'edge',
                        style: {
                            'curve-style': 'haystack',
                            'haystack-radius': 0,
                            'width': 5,
                            'opacity': 0.5,
                            'label': 'data(maxLRS)',
                            'line-color': '#ccc'
                        }
                    }
                ]
            });

            return cy;
        };
    };
};

exports.graphAddCollection = function(cy) {
    return function(eles) {
        return function() {
            return cy.add(eles);
        };
    };
};

exports.graphGetCollection = function(cy) {
    return function() {
        return cy.collection('*');
    };
};

exports.runLayout = function(cy) {
    return function(layout) {
        return function() {
            cy.layout({name: layout}).run();
        };
    };
};

exports.resizeContainer = function(cy) {
    return function() {
        cy.resize();
    };
};

exports.onEventImpl = function(cy) {
    return function(evs) {
        return function(callback) {
            return function() {
                // Need to wrap callback since it's effectful, and PS wraps Eff in nullary functions.
                cy.on(evs, function(e) {
                    callback(e)();
                });
            };
        };
    };
};

exports.parseEventImpl = function(left) {
    return function(right) {
        return function(ev) {
            var result = {};
            result.cy = ev.cy;
            if (ev.cy === ev.target) {
                result.target = right(ev.cy);
            } else {
                result.target = left(ev.target);
            }
            return result;
        };
    };
};

exports.graphRemoveCollection = function(eles) {
    return function() {
        return eles.remove();
    };
};

exports.graphRemoveAll = function(cy) {
    return function() {
        cy.elements().remove();
    };
};
