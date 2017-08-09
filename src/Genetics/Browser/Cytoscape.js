"use strict";

exports.cytoscapeImpl = function(htmlElement, eles) {
    var headlessOpt = false;
    if (htmlElement === null) {
        headlessOpt = true;
    }
    var cy = require("cytoscape")({

        container: htmlElement,
        headless: headlessOpt,

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

exports.graphAddCollectionImpl = function(cy, eles) {
    return cy.add(eles);
};

exports.graphGetCollectionImpl = function(cy) {
    return cy.collection('*');
};

exports.runLayoutImpl = function(cy, layout) {
    cy.layout({name: layout}).run();
};

exports.resizeContainerImpl = function(cy) {
    cy.resize();
};

exports.onEventImpl = function(cy, evs, callback) {
    // Need to wrap callback since it's effectful, and PS wraps Eff in nullary functions.
    cy.on(evs, function(e) {
        callback(e)();
    });
};

exports.parseEventImpl = function(left, right, ev) {
    var result = {};
    result.cy = ev.cy;
    if (ev.cy === ev.target) {
        result.target = right(ev.cy);
    } else {
        result.target = left(ev.target);
    }
    return result;
};

exports.graphRemoveCollectionImpl = function(eles) {
    return eles.remove();
};

exports.graphRemoveAllImpl = function(cy) {
    cy.elements().remove();
};
