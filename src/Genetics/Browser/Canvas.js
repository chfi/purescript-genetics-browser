"use strict";

exports.unsafeCreateElementImpl = function(type, id, className) {
    var c = document.createElement(type);
    c.id = id;
    c.className = className;
    return c;
};

exports.initializeCanvasImpl = function(canvas, size) {
    canvas.width  = size.width;
    canvas.height = size.height;

    var ctx = canvas.getContext("2d");
    if (ctx.imageSmoothingEnabled === true) {
        ctx.imageSmoothingEnabled = false;
    };
};

exports.setElementStyleImpl = function(e,k,v) {
    e.style[k] = v;
}

exports.appendElem = function(cont) {
    return function(canv) {
        return function() {
            cont.appendChild(canv);
        };
    };
}

exports.setContainerStyle = function(e) {
    return function(dim) {
        return function() {
            e.style.position = "relative";
            e.style.border   = "1px solid black";
            e.style.display  = "block";
            e.style.margin   = "0";
            e.style.padding  = "0";
            e.style.width = (dim.width - 2) + "px"; // remove 2px for the border
            e.style.height = dim.height + "px";
        };
    };
};


exports.drawCopies = function(bfr, bfrDim, ctx, pred, ps) {
    var w = Math.round(bfrDim.width);
    var h = Math.round(bfrDim.height);

    ps.forEach(function(p) {
        if (pred(p)) {
            ctx.drawImage(bfr,
                          Math.floor(p.x - (bfrDim.width  / 2.0)),
                          Math.floor(p.y - (bfrDim.height / 2.0)));
        }
    });
};

exports.setCanvasTranslation = function(p) {
    return function(c) {
        return function() {
            var ctx = c.getContext('2d');
            ctx.setTransform(1, 0, 0, 1, p.x, p.y);
        };
    };
};


exports.elementClickImpl = function(el, cb) {
    var rect = el.getBoundingClientRect();
    el.addEventListener('mousedown', function(e) {
        var x = e.clientX - rect.left + window.scrollX;
        var y = e.clientY - rect.top  + window.scrollY;
        cb({x: x, y: y})();
    });
};



// scrolls a canvas, given a "back buffer" canvas to copy the current context to
exports.scrollCanvasImpl = function(backCanvas, canvas, p) {

    var bCtx = backCanvas.getContext('2d');
    bCtx.save();
    bCtx.globalCompositeOperation = "copy";
    bCtx.drawImage(canvas, 0, 0);
    bCtx.restore();

    var ctx = canvas.getContext('2d');

    ctx.save();
    ctx.globalCompositeOperation = "copy";
    ctx.setTransform(1,0,0,1,0,0);
    ctx.drawImage(backCanvas, p.x, p.y);
    ctx.restore();
};


exports.zoomCanvasImpl = function(backCanvas, canvas, edges) {
    var bCtx = backCanvas.getContext('2d');
    bCtx.save();
    bCtx.globalCompositeOperation = "copy";
    bCtx.drawImage(canvas, 0, 0);
    bCtx.restore();

    var ctx = canvas.getContext('2d');

    var width = canvas.width;
    var height = canvas.height;

    var sx = edges.left * width;
    var sWidth = (edges.right - edges.left) * width

    ctx.save();
    ctx.globalCompositeOperation = "copy";
    ctx.setTransform(1,0,0,1,0,0);
    ctx.drawImage(backCanvas,
                  sx, 0.0,
                  sWidth, height,
                  0.0, 0.0,
                  width, height);
    ctx.restore();
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


exports.canvasWheelCBImpl = function(canvas) {
    return function(cb) {
        return function() {
            var evCb = function(e) {
                e.preventDefault();
                cb(Math.sign(e.deltaY))();
            };

            canvas.addEventListener("wheel", evCb);
        }
    };
};
