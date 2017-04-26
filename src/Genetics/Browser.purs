module Genetics.Browser
       where

import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (Renderer)

import Genetics.Browser.Cytoscape as Cytoscape
import Genetics.Browser.Biodalliance as Biodalliance

-- TODO: ugly that the LinePlotConfig is referred to at all outside Lineplot.purs...
qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render


qtlFetch = fetch


cytoscape = Cytoscape.cytoscape
setOn = Cytoscape.setOn
-- setBDOn = Cytoscape.setBDOn

ajaxCytoscape = Cytoscape.ajaxCytoscape
ajaxAddEles = Cytoscape.ajaxAddEles
filterElements = Cytoscape.filterElements
elesOn = Cytoscape.elesOn

addCyFilter = Biodalliance.addCyFilter
addBdScrollCallback = Biodalliance.addCyCallback
