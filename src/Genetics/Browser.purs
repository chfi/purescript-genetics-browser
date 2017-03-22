module Genetics.Browser
       ( qtlGlyphify
       , gwasGlyphify
       ) where

import Genetics.Browser.Renderer.GWAS as GWAS
import Genetics.Browser.Renderer.Lineplot as QTL

qtlGlyphify = QTL.glyphifyFeatures
gwasGlyphify = GWAS.render
