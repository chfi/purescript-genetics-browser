

exports.trackSource1 = { eventName: "event1",
                         eventTemplate: {"data": "String"},
                         rawTemplate: {"x": {"y1": "data",
                                             "y2": "other"}}
                       };

exports.rawEvent1 = {"x": {"y1": "this is a string",
                           "y2": true }
                    };

var sink1Fun = function(json) {
    return json.data || "it broke";
};

exports.trackSink1 = { eventName: "event1",
                       eventFun: sink1Fun };

exports.stringJson = "this is a string";
exports.numberJson = 123.0;
exports.recordJson = { "inner": { pos: 1.23, name: "recordJson" },
                       "number": 456.0 };
