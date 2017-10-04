"use strict";

exports.setBDRef = function(bd) {
    return function() {
        window.bd = bd;
    };
};


// bd can produce ranges
exports.bdTrackSourceConfig = [{ eventName: "range",
                                 eventTemplate: { "chr": "Chr",
                                                  "minPos": "Bp",
                                                  "maxPos": "Bp"
                                                },
                                 rawTemplate: { "chr": "chr",
                                                "minPos": "min",
                                                "maxPos": "max"
                                              }
                               }];


// bd can consume range and location events
var bdConsumeRange = function(json) {
    return function(bd) {
        return function() {
            bd.setLocation(json.chr,
                           json.minPos,
                           json.maxPos);
        };
    };
};


var bdConsumeLoc = function(json) {
    return function(bd) {
        return function() {
            bd.setLocation(json.chr,
                           json.pos - 1000000.0,
                           json.pos + 1000000.0);
        };
    };
};

exports.bdTrackSinkConfig = [ { eventName: "range",
                                eventFun: bdConsumeRange
                              },
                              { eventName: "location",
                                eventFun: bdConsumeLoc
                              }
                            ];


// cy.js can produce locations
exports.cyGraphSourceConfig = [{ eventName: "location",
                                 eventTemplate: { "chr": "Chr",
                                                  "minPos": "Bp",
                                                  "maxPos": "Bp"
                                                },
                                 rawTemplate: { "data": { "lrsLoc": { "chr": "Chr",
                                                                      "pos": "Bp"
                                                                    }
                                                        }
                                              }
                               }];
