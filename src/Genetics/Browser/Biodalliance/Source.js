"use strict";

exports.dummySourceBase = function() {
    return {};
};

exports.createSourceImpl = function(ForeignSourceBase) {
    return function(fetchFun) {
        var Source = {};

        ForeignSourceBase.call(Source);

        Source.fetch = function(chr, min, max, scale, types, pool, callback) {
            fetchFun(chr)(min)(max)().then(function(result) {
                callback(result);
            }, function(error) {
                callback(error);
            });
        };
    };
};
