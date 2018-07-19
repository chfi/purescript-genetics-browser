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
