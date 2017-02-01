{-|
Copyright  : (c) Nathan Bloomfield, 2017
License    : GPL-3
Maintainer : nbloomf@gmail.com
Stability  : experimental

This module provides a filter for Hakyll which expands WordPress-style shortcodes. To use it, include the line @>>= applyShortcodes allServices@ in your compiler.
-}

module Hakyll.Shortcode (
  ShortcodeService(..),
  expandShortcodes,
  applyShortcodes,
  allServices
) where

import Hakyll.Shortcode.Service.GeoGebra
import Hakyll.Shortcode.Service.YouTube
import Hakyll.Shortcode.Service.Example


-- | A simple sum type representing the available shortcodes.
data ShortcodeService
  = GeoGebra
  | YouTube
  | Example
  deriving Eq


-- Expand shortcodes of the provided type.
expandShortcodesFor :: ShortcodeService -> String -> String
expandShortcodesFor x = case x of
  GeoGebra -> expandGeoGebraShortcodes
  YouTube  -> expandYouTubeShortcodes
  Example  -> expandExampleShortcodes


-- | Expand shortcodes of each of the provided types.
expandShortcodes :: [ShortcodeService] -> String -> String
expandShortcodes = foldr1 (.) . map expandShortcodesFor


-- | Monadic version of 'expandShortcodes', for use with Hakyll.
applyShortcodes :: (Monad m, Functor f)
  => [ShortcodeService] -> f String -> m (f String)
applyShortcodes svc text =
  return $ (fmap $ expandShortcodes svc) text


-- | A list of all the available shortcodes (for convenience).
allServices :: [ShortcodeService]
allServices =
  [ GeoGebra
  , YouTube
  ]
