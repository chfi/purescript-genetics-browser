"use strict";

exports.setBDRef = function(bd) {
    return function() {
        window.bd = bd;
    };
};
