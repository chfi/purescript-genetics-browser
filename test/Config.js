"use strict";

var wrapRenderer = function() {
    console.log("wrapRenderer!");
};

var browser = function() {
    console.log("browser!");
};



var bdSources = [
    {
        name: 'Genome',
        twoBitURI: 'http:www.biodalliance.org/datasets/GRCm38/mm10.2bit',
        desc: 'Mouse reference genome build GRCm38',
        tier_type: 'sequence',
        provides_entrypoints: true
    },
    {
        name: 'QTL',
        tier_type: 'qtl',
        renderer: "qtlRenderer",
        uri: 'http:test-gn2.genenetwork.org/api_pre1/qtl/lod2.csv'
    }
];

var cySources = [ undefined ];

exports.validConfig = { wrapRenderer: wrapRenderer,
                        browser: browser,
                        tracks: { BDTrack: bdSources,
                                  CyGraph: cySources
                                }
                      };

exports.validNoBD = { tracks: { CyGraph: cySources } };

exports.badConfigWrapRenderer = { wrapRenderer: "not a function",
                                  browser: browser,
                                  tracks: { BDTrack: bdSources,
                                            CyGraph: cySources
                                          }
                                };

exports.badConfigBrowser = { wrapRenderer: wrapRenderer,
                             browser: undefined,
                             tracks: { BDTrack: bdSources,
                                       CyGraph: cySources
                                     }
                           };

exports.badConfigTracks1 = { wrapRenderer: wrapRenderer,
                             browser: browser,
                             tracks: { track: {}
                                     }
                           };


exports.badConfigTracks2 = { wrapRenderer: wrapRenderer,
                             browser: browser,
                             tracks: {}
                           };
