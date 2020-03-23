
exports.unsafeCreateElementImpl = function(type, id) {
    var c = document.createElement(type);
    c.id = id;
    return c;
};

exports.unsafeCreateElementNSImpl = function(type, id, ns) {
  var c = document.createElementNS(ns, type);
  c.id = id;
  return c;
};


exports.appendElem = function(cont) {
    return function(canv) {
        return function() {
            cont.appendChild(canv);
        };
    };
}
