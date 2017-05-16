
exports.addFeatureListener = function(bd) {
    return function(callback) {
        return function() {
            bd.addFeatureListener(function(ev, feature, hit, tier) {
                console.log("clicked on: ");
                console.log(feature);
                callback(feature)();
            });
        };
    };
};


exports.setLocationImpl = function(bd) {
    return function(chr) {
        return function(xl) {
            return function(xr) {
                return function() {
                    bd.setLocation(chr.slice(3), xl, xr);
                };
            };
        };
    };
};

exports.scrollViewImpl = function(bd) {
    return function(x) {
        return function() {
            var xl = (bd.viewStart|0) + x;
            var xr = (bd.viewEnd|0) + x;
            bd.setLocation(null, xl, xr);
        };
    };
};

exports.parseBDFeaturePos = function(feature) {
    return { chr: feature.chr,
             min: feature.min,
             max: feature.max
           };
};
