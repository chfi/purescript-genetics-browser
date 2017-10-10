"use strict";

exports.animationFrameLoop = function(cb) {
    return function() {
        var step = function() {
            cb();
            window.requestAnimationFrame(step)
        };
        window.requestAnimationFrame(step);
    };
};

exports.clearCanvas = function(w) {
    return function(h) {
        return function(ctx) {
            return function() {
                ctx.clearRect(0, 0, w, h);
            };
        };
    };
};

exports.setButtonEvent = function(id) {
    return function(sub) {
        return function() {
            var el = document.getElementById(id);
            el.addEventListener('click', function(ev) {
                sub();
            });
        };
    };
};

exports.getScreenSize = function() {
    var w = window.innerWidth;
    var h = window.innerHeight;
    return { w: w, h: h };
};
