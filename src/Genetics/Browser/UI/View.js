"use strict";


exports.onTimeout = function(delay) {
    return function(cb) {
        return function() {
            var timeoutRef = null;

            var throttled = function() {
                if (timeoutRef) {
                    clearTimeout(timeoutRef);
                }

                timeoutRef = setTimeout(function() {
                    timeoutRef = null;
                    cb();
                }, delay);
            };

            return { run: throttled,
                     cancel: function() { clearTimeout(timeoutRef) } };
        };
    };
};


exports.onFrame = function(cb) {
    return function() {

        var cbRef = null;

        var runCallback = function() {
            if (cbRef) {
                window.cancelAnimationFrame(cbRef);
            }

            cbRef = window.requestAnimationFrame(function(t) {
                cb();
                cbRef = null;
            });
        };

        var cancelCallback = function() {
            if (cbRef) {
                window.cancelAnimationFrame(cbRef);
                cbRef = null;
            }
        };

        return { run: runCallback, cancel: cancelCallback };
    };
};
