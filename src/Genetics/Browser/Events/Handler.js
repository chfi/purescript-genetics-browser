exports.mergeRecord = function(r1) {
    return function(r2) {
        var r3 = {};
        for (k in r1) {
            r3.k = r1.k;
        }
        for (k in r2) {
            r3.k = r2.k;
        }
        return r3;
    };
};
