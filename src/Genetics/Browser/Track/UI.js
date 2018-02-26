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

exports.getScreenSize = function() {
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

exports.drawImageMany = function(bfr, ctx, dim, ps) {
    ps.forEach(function(p) {
        ctx.drawImage(bfr, 0, 0, dim.width, dim.height, p.x, p.y, dim.width, dim.height);
    });
};


// scrolls a canvas, given a "back buffer" canvas to copy the current context to
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

                ctx.save();
                ctx.setTransform(1,0,0,1,0,0);
                ctx.fillStyle = 'white';
                ctx.fillRect(0, 0, canvas.width, canvas.height);
                ctx.drawImage(backCanvas, p.x, p.y);
                ctx.restore();

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


exports.canvasDragImpl = function(canvas) {
    return function(cb) {
        return function() {
            var cbInner = function(e) {
                var startX = e.clientX;
                var startY = e.clientY;
                var lastX = e.clientX;
                var lastY = e.clientY;

                var f = function(e2) {
                    cb({during: {x: lastX - e2.clientX, y: lastY - e2.clientY}})();
                    lastX = e2.clientX;
                    lastY = e2.clientY;
                };

                document.addEventListener('mousemove', f);

                document.addEventListener('mouseup', function(e2) {
                    document.removeEventListener('mousemove', f);
                    cb({total: {x: e2.clientX-startX, y: e2.clientY-startY}})();
                }, { once: true });
            };

            canvas.addEventListener('mousedown', cbInner);
            return function() {
                canvas.removeEventListener('mousedown', cbInner);
            }
        };
    };
};

exports.setViewUI = function(contents) {
    return function() {
        var el = document.getElementById("view");
        el.innerHTML = contents;
    };
};


exports.canvasWheelCBImpl = function(canvas) {
    return function(cb) {
        return function() {
            var evCb = function(e) {
                // hack to be sorta-compatible with both line and pixel modes
                if (e.deltaMode === 0) {
                    cb(e.deltaY * 0.1)();
                } else {
                    cb(e.deltaY)();
                }
            };

            canvas.addEventListener("wheel", evCb);
        }
    };
};
