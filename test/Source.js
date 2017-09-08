"use strict";

exports.testFetch = function(source) {
    return function() {
        var out = source.fetch("11", 10, 20, null, null, null, function(err, res) {
            console.log(res);
        });
    };
};
