"use strict";

exports.createSourceImpl = function(fetchFun) {
    var Source = {};

    Source.fetch = function(chr, min, max, scale, types, pool, callback) {
        fetchFun(chr)(min)(max)().then(function(result) {
            callback(null, result);
        }, function(error) {
            callback(error);
        });
    };

    return Source;
};
