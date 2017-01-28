module Hakyll.Shortcode (
  ShortcodeService(..),
  expandShortcodes,
  expandAllShortcodes
) where

import Hakyll.Shortcode.Service.GeoGebra
import Hakyll.Shortcode.Service.YouTube

expandAllShortcodes :: String -> String
expandAllShortcodes = foldr1 (.)
  [ expandShortcodes GeoGebra
  , expandShortcodes YouTube
  ]

expandShortcodes :: ShortcodeService -> String -> String
expandShortcodes x = case x of
  GeoGebra -> expandGeoGebraShortcodes
  YouTube  -> expandYouTubeShortcodes

data ShortcodeService
  = GeoGebra
  | YouTube
  deriving (Eq, Enum)
