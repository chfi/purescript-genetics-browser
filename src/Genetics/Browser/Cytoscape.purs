module Genetics.Browser.Cytoscape where

foreign import data Cytoscape :: Type

-- TODO should be an Eff
foreign import cytoscape :: String -> Cytoscape
