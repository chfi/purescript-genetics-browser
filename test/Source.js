"use strict";

exports.testFetch = function(source) {
    return function() {
        return source.fetch("11", 10, 20);
    };
};
