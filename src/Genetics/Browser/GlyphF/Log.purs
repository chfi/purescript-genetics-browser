module Genetics.Browser.GlyphF.Log
       ( showGlyph )
       where

import Prelude

import Control.Monad.Free (foldFree)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Traversable (traverse_)
import Genetics.Browser.Glyph (Glyph)
import Genetics.Browser.GlyphF (GlyphF(..))


glyphLogN :: GlyphF ~> Writer String
glyphLogN (Stroke c a)   = do
  tell $ "Set stroke style to " <> c
  pure a
glyphLogN (Fill c a)     = do
  tell $ "Set fill style to " <> c
  pure a
glyphLogN (Circle p r a) = do
  tell $ "Drawing circle at (" <> show p.x <> ", " <> show p.y <>
         ") with radius " <> show r <> "."
  pure a
glyphLogN (Line p1 p2 a) = do
  tell $ "Drawing line from (" <> show p1.x <> ", " <> show p1.y <> ") to " <>
         "(" <> show p2.x <> ", " <> show p2.y <> ")"
  pure a
glyphLogN (Rect p1 p2 a) = do
  tell $ "Drawing rectangle from (" <> show p1.x <> ", " <> show p1.y <> ") to " <>
         "(" <> show p2.x <> ", " <> show p2.y <> ")"
  pure a
glyphLogN (Path ps a) = do
  tell $ "Drawing a path:"
  traverse_ (\p -> tell ("(" <> show p.x <> ", " <> show p.y <> ")")) ps
  pure a


showGlyph :: ∀ a. Glyph a -> String
showGlyph = execWriter <<< foldFree glyphLogN
