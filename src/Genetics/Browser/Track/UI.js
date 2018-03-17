"use strict";

exports.buttonEvent = function(id) {
    return function(cb) {
        return function() {
            var el = document.getElementById(id);
            el.addEventListener('click', function(ev) {
                cb();
            });
        }
    };
};

exports.windowInnerSize = function() {
    var w = window.innerWidth;
    var h = window.innerHeight;
    return { width: w, height: h };
};

exports.timeEff = function(name) {
    return function() {
        console.time(name);
        return function() {
            console.timeEnd(name);
        }
    };
};


exports.timeFun = function(f) {
    return function() {
        console.time();
        f();
        console.timeEnd();
    };
};

exports.setWindow = function(k) {
    return function(v) {
        return function() {
            window[k] = v;
        };
    };
};
