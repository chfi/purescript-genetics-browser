exports.cytoscape = function(div) {
    return function(eles) {
        var cy = require("cytoscape")({

        container: document.getElementById(div), // container to render in

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
                    // 'target-arrow-color': '#ccc',
                    // 'target-arrow-shape': 'triangle'
                }
            }
        ],

        layout: {
            name: 'circle'
        }

        });
        return cy;
    };
};

exports.setOn = function(cy) {
    return function(evs) {
        return function(callback) {
            return function() {
                cy.on(evs, callback);
            };
        };
    };
};

exports.setBDOn = function(bd) {
    return function(cy) {
        cy.on('tap', function(evt){
            console.log(evt.target.id);
            console.log(bd);
            if (evt.target.id() === 'a') {
                console.log("going left");
                bd.setLocation(null, 6400000, 10000000);
            } else if (evt.target.id() === 'b') {
                console.log("going right");
                bd.setLocation(null, 14000000, 20000000);
            }
        });
    };
};

exports.elesOn = function(evs) {
    return function(callback) {
        return function(eles) {
            return function() {
                eles.on(evs, callback);
            };
        };
    };
};

exports.cyAdd = function(cy) {
    return function(eles) {
        return cy.add(eles);
    };
};

exports.removeElements = function(eles) {
    return function() {
        return eles.remove();
    };
};

exports.filterElements = function(pred) {
    return function(eles) {
        return eles.filter(pred);
    };
};
