module Genetics.Browser
       ( qtlGlyphify
       , qtlFetch
       , gwasGlyphify
       , module Genetics.Browser.Cytoscape
       )
       where

import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL
import Genetics.Browser.Renderer.Lineplot (LinePlotConfig)
import Genetics.Browser.Source.QTL (fetch)
import Genetics.Browser.Types (Renderer)

import Genetics.Browser.Cytoscape

-- TODO: ugly that the LinePlotConfig is referred to at all outside Lineplot.purs...
qtlGlyphify :: LinePlotConfig -> Renderer
qtlGlyphify = QTL.render

gwasGlyphify :: Renderer
gwasGlyphify = GWAS.render


qtlFetch = fetch
