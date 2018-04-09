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


var infoBoxId = "view";

exports.setInfoBoxVisibility = function(vis) {
    return function() {
        var infoBox = document.getElementById(infoBoxId);
        infoBox.style['visibility'] = vis;
    };
};

exports.setInfoBoxContents = function(html) {
    return function() {
        var infoBox = document.getElementById(infoBoxId);
        infoBox.innerHTML = html;
    };
};


var debugDivId = "debugDiv";

exports.initDebugDiv = function(radius) {
    return function() {
        var view = document.getElementById("browser");
        var div = document.getElementById(debugDivId);
        if (!div) {
            div = document.createElement("div");
            view.appendChild(div);
        }
        div.id = debugDivId;

        div.style['position'] = "relative";
        div.style['left'] = "0.0";
        div.style['top']  = "0.0";
        div.style['border-radius'] = "50%";
        div.style['width']         = (radius * 2.0) + "px";
        div.style['height']        = (radius * 2.0) + "px";
        div.style['z-index']       = "100";
        div.style['backgroundColor'] = "red";
        div.style['pointer-events'] = "none";
        div.style['display'] = "inline-block";
        div.style['visibility'] = "hidden";
        div.dataset.radius = radius;
        return div;
    };
};

var getDebugDiv = function() {
    var div = document.getElementById(debugDivId);
    if (!div) {
        return initDebugDiv(10.0)();
    } else {
        return div;
    }
};

exports.setDebugDivVisibility = function(s) {
    return function() {
        var div = getDebugDiv();
        div.style['visibility'] = s;
    };
};


exports.setDebugDivPoint = function(p) {
    return function() {
        var div = getDebugDiv();
        var r = div.dataset.radius | 1.0;
        var x = p.x - r;
        var y = p.y - r * 2.0;
        // var y = p.y;
        div.style['left'] = x + "px";
        div.style['top']  = y + "px";
    };
};
