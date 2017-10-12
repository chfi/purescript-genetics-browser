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


// scrolls a canvas, given a "back buffer" canvas to copy the current context to
// TODO: later, do all heavy lifting on the backcanvas and just draw that to the screen
exports.scrollCanvas = function(backCanvas) {
    return function(canvas) {
        return function(p) {
            return function() {
                // for some reason, doing this in newCanvas() below doesn't stick
                backCanvas.width = canvas.width;
                backCanvas.height = canvas.height;

                var bCtx = backCanvas.getContext('2d');
                var ctx = canvas.getContext('2d');

                bCtx.drawImage(canvas, 0, 0);

                // clears the canvas...
                // no idea of performance vs. drawRect
                canvas.width = canvas.width;
                ctx.drawImage(backCanvas, p.x, p.y);
            };

        };
    };
};


exports.newCanvas = function(size) {
    return function() {
        var c = document.createElement('canvas');
        return c;
    };
};

var evToPoint = function(e) {
    return { x: e.clientX,
             y: e.clientY
           };
}

var canvasEvent = function(type) {
    return function(canvas) {
        return function(sub) {
            var cb = function(e) {
                sub(evToPoint(e));
            };

            canvas.addEventListener(type, cb);
            return function() {
                removeEventListener(type, cb);
            }
        };
    };
};


var canvasClick = canvasEvent("click");

var canvasMouseup = canvasEvent("mouseup");

var canvasMousedown = canvasEvent("mousedown");

var canvasDrag = function(canvas) {
    var during = function(sub) {
        var cb = function(e) {
            var lastX = e.clientX;
            var lastY = e.clientY;

            var f = function(e2) {
                sub({x: lastX - e2.clientX, y: lastY - e2.clientY});
                lastX = e2.clientX;
                lastY = e2.clientY;
            };

            document.addEventListener('mousemove', f);

            document.addEventListener('mouseup', function(e2) {
                document.removeEventListener('mousemove', f);
            }, { once: true });
        };

        canvas.addEventListener('mousedown', cb);
        return function() {
            canvas.removeEventListener('mousedown', cb);
        }
    };
};

exports.canvasDrag = canvasDrag;


exports.canvasEvents = function(canvas) {
    return { click: canvasClick(canvas),
             mouseup: canvasMouseup(canvas),
             mousedown: canvasMousedown(canvas),
             drag: exports.canvasDrag(canvas)
           };
};
