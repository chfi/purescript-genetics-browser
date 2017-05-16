exports.cytoscape = function(el) {
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
                            // 'target-arrow-color': '#ccc',
                            // 'target-arrow-shape': 'triangle'
                        }
                    }
                ],
            });
            return cy;
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

exports.cyFilter = function(pred) {
    return function(cy) {
        return cy.filter(pred);
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

exports.cyFilterElements = function(cy) {
    return function(pred) {
        return function() {
            return cy.filter(pred);
        };
    };
};

exports.cyFilterByString = function(cy) {
    return function(chr) {
        return function() {
            var es = cy.filter(function(e,y) {
                return e.group() == "edges" && e.data().lrsLoc.chr != ("Chr" + chr);
            });
            es.remove()
                .targets()
                .remove();
        };
    };
};

exports.elDataImpl = function(just) {
    return function(nothing) {
        return function(el) {
            var loc = el.data().lrsLoc;
            console.log(el);
            console.log("element data:");
            console.log(el.data());
            if (loc.chr && loc.pos) {
                return just({chr: loc.chr,
                             pos: loc.pos
                            });
            } else {
                return nothing;
            }
        };
    };
};


exports.evtTarget = function(evt) {
    return evt.target;
};
