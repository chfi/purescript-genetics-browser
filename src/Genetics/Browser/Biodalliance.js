"use strict";

exports.initBDimpl = function(opts) {
    return function(wrapRenderer) {
        return function(browser) {
            return function(el) {
                return function() {
                    var renderers = {};
                    opts.renderers.forEach(function(r) {
                        renderers[r.name] = wrapRenderer(r.renderer, r.canvasHeight);
                    });

                    // TODO: make sure to only add BD tracks when that becomes relevant
                    var sources = opts.sources;

                    var b = new browser({

                        injectionPoint: el,

                        // these should be set to default and updated from the main container;
                        // BD shouldn't do any scrolling on its own
                        chr:        '11',
                        defaultStart: 10000000,
                        defaultEnd: 30000000,
                        viewStart:  10000000,
                        viewEnd:    30000000,

                        // Can this be changed dynamically?
                        // It should be controlled by PS, too.
                        // Maybe it doesn't matter if we're controlling the viewport w/ the API
                        maxViewWidth: 20000000,

                        // this should be set externally as well
                        cookieKey:  'mouse38',

                        // these should be set based on the track configs in opts,
                        // but that's probably overkill right now...
                        externalRenderers: renderers,
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

                        sources: sources,
                        // sources: [
                        //     {
                        //         name: 'Genome',
                        //         twoBitURI: 'http://www.biodalliance.org/datasets/GRCm38/mm10.2bit',
                        //         desc: 'Mouse reference genome build GRCm38',
                        //         tier_type: 'sequence',
                        //         provides_entrypoints: true
                        //     },
                        //     {
                        //         name: 'GWAS',
                        //         renderer: "gwasRenderer",
                        //         "forceReduction": -1,
                        //         bwgURI: 'http://localhost:8080/gwascatalog.bb'
                        //     },
                        //     {
                        //         name: 'QTL',
                        //         tier_type: 'qtl',
                        //         renderer: "qtlRenderer",
                        //         uri: 'http://test-gn2.genenetwork.org/api_pre1/qtl/lod2.csv'
                        //     }
                        // ],
                        /*uiPrefix: '../',*/

                        fullScreen: true
                    });

                    return b;
                };
            };
        };
    };
};


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
