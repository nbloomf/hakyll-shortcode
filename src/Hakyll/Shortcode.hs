module Hakyll.Shortcode (
  expandAllShortcodes,
  expandYouTubeShortcodes
) where

import Hakyll.Shortcode.Service.YouTube

expandAllShortcodes :: String -> String
expandAllShortcodes = foldr1 (.)
  [ expandYouTubeShortcodes
  ]
