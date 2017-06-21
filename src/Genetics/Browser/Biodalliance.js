"use strict";

exports.initBD = function(opts) {
    return function(browser) {
        return function(el) {
            return function() {
                var b = new browser({

                    injectionPoint: el,


                    // these should be set to default and updated from the main container;
                    // BD shouldn't do any scrolling on its own
                    chr:        '11',
                    defaultStart: 10000000,
                    defaultEnd: 30000000,
                    viewStart:  10000000,
                    viewEnd:    30000000,
                    maxViewWidth: 20000000,

                    // this should be uniquely generated?
                    cookieKey:  'mouse38',

                    // these should be set based on the track configs in opts,
                    // but that's probably overkill right now...
                    externalRenderers: opts.externalRenderers,
                    // externalRenderers: { gwasRenderer: gwasRenderer,
                    //                      qtlRenderer: qtlRenderer
                    //                    },

                    // idk how best to do this. for now let it be hardcoded,
                    // later send as plain JSON, maybe use a function to generate it
                    coordSystem: {
                        speciesName: 'Mouse',
                        taxon: 10090,
                        auth: 'GRCm',
                        version: 38,
                        ucscName: 'mm10'
                    },

                    // workers/threading should be configurable in the main container
                    maxWorkers: 0,

                    // sources: opts.sources,
                    sources: [
                        {
                            name: 'Genome',
                            twoBitURI: 'http://www.biodalliance.org/datasets/GRCm38/mm10.2bit',
                            desc: 'Mouse reference genome build GRCm38',
                            tier_type: 'sequence',
                            provides_entrypoints: true
                        },
                        {
                            name: 'GWAS',
                            renderer: "gwasRenderer",
                            sub: {
                                multi_id: "multi_1",
                                offset: 0.0
                            },
                            "forceReduction": -1,
                            bwgURI: 'http://localhost:8080/gwascatalog.bb'
                        },
                        {
                            name: 'QTL',
                            tier_type: 'qtl',
                            renderer: "qtlRenderer",
                            sub: {
                                multi_id: "multi_1",
                                offset: 0.0
                            },
                            uri: 'http://test-gn2.genenetwork.org/api_pre1/qtl/lod2.csv'
                        },
                        {
                            name: 'QTL, GWAS',
                            tier_type: 'multi',
                            renderer: 'multi',
                            multi: {
                                multi_id: "multi_1",
                                grid: true,
                                grid_offset: 0,
                                grid_spacing: 10
                            }
                        }
                    ],
                    /*uiPrefix: '../',*/

                    fullScreen: true
                });

                return b;
            };
        };
    };
};


// exports.createTrackConfig = function(config) {
//     return { name: config.name,
//              }
// };

exports.addFeatureListener = function(bd) {
    return function(callback) {
        return function() {
            bd.addFeatureListener(function(ev, feature, hit, tier) {
                callback(feature)();
            });
        };
    };
};

exports.addInitListener = function(bd) {
    return function(callback) {
        return function() {
            bd.addInitListener(function() {
                callback();
            });
        };
    };
};

exports.setLocationImpl = function(bd) {
    return function(chr) {
        return function(xl) {
            return function(xr) {
                return function() {
                    bd.setLocation(chr, xl, xr);
                };
            };
        };
    };
};

exports.scrollViewImpl = function(bd) {
    return function(x) {
        return function() {
            var xl = (bd.viewStart|0) + x;
            var xr = (bd.viewEnd|0) + x;
            bd.setLocation(null, xl, xr);
        };
    };
};
