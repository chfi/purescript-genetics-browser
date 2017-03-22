exports.testGlyphPos = function(g) {
    console.log(g.min());
    console.log(g.max());
    console.log(g.minY());
    console.log(g.maxY());
};


exports.showGlyphSVG = function(g) {
    console.log("Printing SVG");
    var e = g.toSVG();
    console.log(e);
};

exports.addElementToDiv = function(divId) {
    return function(e) {
        return function () {
            console.log("Adding SVG to div #" + divId);
            var d = document.getElementById(divId);

            console.log(e);
            d.appendChild(e);
        };
    };
};


exports.setOnLoad = function(f) {
    return function() {
        var window = window | null;
        if (null !== window)
            window.onload = f;
        else
            console.log("Test not running in browser, skipping");
    };
};
