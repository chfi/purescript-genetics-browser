
const tubemap = require('../../js/tubemap.js');

exports.extractNodes = function(vg) {
  return tubemap.vgExtractNodes(vg);
};

exports.extractTracks = function(vg) {
  return tubemap.vgExtractTracks(vg);
};

exports.extractReads = function(nodes) {
  return function(tracks) {
    return function(reads) {
      return tubemap.vgExtractReads(nodes, tracks, reads);
    };
  };
};

exports.tubemap = tubemap;



exports.createTubeMapImpl = function(params) {
  return function() {
    tubemap.create(params)
  };
};

exports.isInitialized = function() {
  return tubemap.zoom !== undefined;
};
