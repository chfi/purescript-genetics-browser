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
