"use strict";

var idb = require('idb-keyval');

const gbStore = new idb.Store('genetics-browser-db', 'genetics-browser-store');

exports.setValueImpl = function(key) {
    return function(value) {
        return function() {
            idb.set(key, value, gbStore)
                .then(() => console.log("successfully set " + value))
                .catch(err => console.log("failure: ", err));

            return true;
        };
    };
};

exports.setValueImplCB = function(key) {
    return function(value) {
        return function(onsuccess) {
            return function(onfailure) {
                return function() {
                    idb.set(key, value, gbStore)
                        .then(onsuccess())
                        .catch(err => onfailure(err)());
                };
            };
        };
    };
};

exports.getValueImplCB = function(key) {
    return function(onsuccess) {
        return function(onfailure) {
            return function() {
                idb.get(key, gbStore)
                    .then(val => onsuccess(val)())
                    .catch(err => onfailure(err)());
            };
        };
    };
};


exports.getValueImpl = function(key) {
    return function(callback) {
        return function() {
            idb.get(key, gbStore).then((v) => callback(v)());

        };
    };
};

exports.keysImpl = function(callback) {
    return function() {
        idb.keys(gbStore).then((keys) => callback(keys)());
    }
};

exports.delImpl = function(key) {
    return function() {
        idb.del(key, gbStore);
    };
};



// foreign import setValue :: String
//                         -> a
//                         -> Effect Boolean
