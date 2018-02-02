"use strict";

exports.clearCanvas = function(canvas) {
    return function() {
        canvas.width = canvas.width;
    };
};

exports.buttonEvent = function(id) {
    return function(sub) {
        var el = document.getElementById(id);
        el.addEventListener('click', function(ev) {
            sub();
        });
    };
};

exports.getScreenSize = function() {
    var w = window.innerWidth;
    var h = window.innerHeight;
    return { width: w, height: h };
};


exports.timeFun = function(f) {
    return function() {
        console.time();
        f();
        console.timeEnd();
    }
}


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
        c.width  = size.width;
        c.height = size.height;
        return c;
    };
};

var evToPoint = function(e) {
    return { x: e.clientX,
             y: e.clientY
           };
}

exports.canvasEvent = function(type) {
    return function(canvas) {
        return function(sub) {
            var cb = function(e) {
                sub(evToPoint(e));
            };

            canvas.addEventListener(type, cb);
            return function() {
                canvas.removeEventListener(type, cb);
            }
        };
    };
};


exports.canvasDragImpl = function(canvas) {
    return function(sub) {
        var cb = function(e) {
            var startX = e.clientX;
            var startY = e.clientY;
            var lastX = e.clientX;
            var lastY = e.clientY;

            var f = function(e2) {
                sub({during: {x: lastX - e2.clientX, y: lastY - e2.clientY}});
                lastX = e2.clientX;
                lastY = e2.clientY;
            };

            document.addEventListener('mousemove', f);

            document.addEventListener('mouseup', function(e2) {
                document.removeEventListener('mousemove', f);
                sub({total: {x: e2.clientX-startX, y: e2.clientY-startY}});
            }, { once: true });
        };

        canvas.addEventListener('mousedown', cb);
        return function() {
            canvas.removeEventListener('mousedown', cb);
        }
    };
};

exports.setViewUI = function(contents) {
    return function() {
        var el = document.getElementById("view");
        el.innerHTML = contents;
    };
};


exports.canvasWheelEvent = function(canvas) {
    return function(sub) {
        var cb = function(e) {
            sub(e.deltaY);
        };

        canvas.addEventListener("wheel", cb);
        return function() {
            canvas.removeEventListener("wheel", cb);
        }
    };
};
