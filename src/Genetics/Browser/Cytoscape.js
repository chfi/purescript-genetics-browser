"use strict";

exports.cytoscapeImpl = function(el) {
    return function(eles) {
        return function() {
            var cy = require("cytoscape")({

                container: el, // container to render in

                elements: eles,

                style: [ // the stylesheet for the graph
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

exports.coreAddCollection = function(cy) {
    return function(eles) {
        return function() {
            return cy.add(eles);
        };
    };
};

exports.coreFilterImpl = function(cy) {
    return function(pred) {
        return function() {
            return cy.filter(pred);
        };
    };
};

exports.runLayout = function(cy) {
    return function(layout) {
        return function() {
            cy.layout({name: layout}).run();
        };
    };
};

exports.resize = function(cy) {
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

exports.collRemoveElements = function(eles) {
    return function() {
        return eles.remove();
    };
};

exports.coreRemoveAllElements = function(cy) {
    return function() {
        cy.elements().remove();
    };
};


exports.eleGetAllData = function(ele) {
    return function() {
        return ele.data();
    };
};

exports.eleGetDataImpl = function(ele) {
    return function(key) {
        return function() {
            return ele.data(key);
        };
    };
};


exports.filterNodes = function(cy) {
    return function(pred) {
        return function() {
            return cy.nodes().filter(function(ele, ind) {
                return pred(ele.data());
            });
        };
    };
};



exports.filterEdgesWithNodes = function(cy) {
    return function(pred) {
        return function() {
            var edges = cy.edges().filter(function(ele, ind) {
                return pred(ele.data());
            });
            var nodes = edges.connectedNodes();
            return edges.union(nodes);
        };
    };
};
