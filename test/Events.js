"use strict";

exports.rangeSourceConfig = { eventName: "range",
                              eventTemplate: { "chr": "Chr",
                                               "minPos": "Bp",
                                               "maxPos": "Bp"
                                             },
                              rawTemplate: { "chr": "chr",
                                             "min": "minPos",
                                             "max": "maxPos"
                                           }
                            };


exports.badRangeConfig = { eventName: "range",
                           eventTemplate: { "chr": "Chr",
                                            "minPos": "Bp",
                                            "maxPos": "Bp"
                                          },
                           rawTemplate: { "chr": "chr",
                                          "min": "minPos"
                                        }
                         };

exports.rawRange1 = { "chr": "11", "min": 1000000, "max": 2000000 };
exports.rawRange2 = { "chr": "3", "min": 3000000, "max": 5000000, "other": true };

exports.parsedRange1 =  {"name": "range",
                         "evData": { "chr": "11", "minPos": 1000000, "maxPos": 2000000 }};

exports.parsedRange2 =  {"name": "range",
                         "evData": { "chr": "3", "minPos": 3000000, "maxPos": 5000000 }};


exports.rawEvent1 = {"x": {"y1": "this is a string",
                           "y2": true }
                    };

var sink1Fun = function(json) {
    return json.data || "it broke";
};

exports.sinkConfig1 = { eventName: "event1",
                        eventFun: sink1Fun };
