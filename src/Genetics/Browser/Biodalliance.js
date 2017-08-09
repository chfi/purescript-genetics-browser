"use strict";

exports.initBDimpl = function(opts, wrapRenderer, browser) {
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

                // All the maxHeight does is set the BD holder CSS...
                // Should (somehow) be controlled by PS.
                // maxHeight: opts.maxHeight || 1000,
                maxHeight: 1000000,

                // BD remembering anything will probably only lead to
                // inconsistencies with the main container
                noPersist: true,

                // these should be set based on the track configs in opts,
                // but that's probably overkill right now...
                externalRenderers: renderers,

                // idk how best to do this. for now let it be hardcoded,
                // later send as plain JSON, maybe use a function to generate it.
                // the first four are mandatory;
                // `taxon` should be an integer from the NCBI taxonomy,
                // `auth` and `version` should match authorty and version strings
                // used in the DAS Registry
                // however the DAS Registry isn't a thing anymore
                // given how we use BD, I think we can skip most of it
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
                /*uiPrefix: '../',*/

                fullScreen: true
            });

            return b;
        };
    };
};


exports.addFeatureListenerImpl = function(bd, callback) {
    bd.addFeatureListener(function(ev, feature, hit, tier) {
        callback(feature)();
    });
};

exports.addInitListener = function(bd, callback) {
    bd.addInitListener(function() {
        callback();
    });
};

exports.setLocationImpl = function(bd, chr, xl, xr) {
    bd.setLocation(chr, xl, xr);
};

exports.scrollViewImpl = function(bd, x) {
    var xl = (bd.viewStart|0) + x;
    var xr = (bd.viewEnd|0) + x;
    bd.setLocation(null, xl, xr);
};
