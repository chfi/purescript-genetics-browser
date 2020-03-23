const GenomeBrowser = require('./output/Genetics.Browser.UI');

window.GenomeBrowser = GenomeBrowser;

const tubemap = require('./js/tubemap.js');
const examplevg = require('./example-vg.json');

window.tubemap = tubemap;
window.examplevg = examplevg;
window.vgnodes = tubemap.vgExtractNodes(examplevg.graph);
window.vgtracks = tubemap.vgExtractTracks(examplevg.graph);
window.vgreads = tubemap.vgExtractReads(window.vgnodes, window.vgtracks, examplevg.gam);

import * as d3 from 'd3';

window.d3 = d3;

window.GBTM = require('./output/Genetics.Browser.TubeMap');
