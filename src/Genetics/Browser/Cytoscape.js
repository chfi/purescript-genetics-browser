exports.cytoscape = function(div) {
    var cy = require("cytoscape")({

        container: document.getElementById(div), // container to render in

        elements: [ // list of graph elements to start with
            { // node a
                data: { id: 'a' }
            },
            { // node b
                data: { id: 'b' }
            },
            { // edge ab
                data: { id: 'ab', source: 'a', target: 'b' }
            }
        ],

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
                    'width': 3,
                    'line-color': '#ccc',
                    'target-arrow-color': '#ccc',
                    'target-arrow-shape': 'triangle'
                }
            }
        ],

        layout: {
            name: 'grid',
            rows: 1
        }

    });

    return cy;
};

exports.setOn = function(cy) {
    cy.on('tap', function(evt){
        console.log(evt);
        console.log( 'tap ' + evt.target.id() );
    });
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
