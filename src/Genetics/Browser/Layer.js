"use strict";

exports.setContextTranslation = function(p) {
    return function(ctx) {
        return function() {
            ctx.setTransform(1, 0, 0, 1, p.x, p.y);
        };
    };
};
