module Hakyll.Shortcode.Service.GeoGebra(
  expandGeoGebraShortcodes
) where

import Hakyll.Shortcode.Service
import Hakyll.Shortcode.Render
import Hakyll.Shortcode.Types


data GeoGebraEmbed = GeoGebraEmbed
  -- String Properties
  { gg_id :: Maybe Letters_Numbers
  } deriving Show


expandGeoGebraShortcodes :: String -> String
expandGeoGebraShortcodes =
  expandShortcodes (emptycode :: GeoGebraEmbed)


instance Shortcode GeoGebraEmbed where



