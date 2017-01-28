{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hakyll.Shortcode.Service.GeoGebra(
  expandGeoGebraShortcodes
) where

import Hakyll.Shortcode.Service
import Hakyll.Shortcode.Render
import Hakyll.Shortcode.Types

import Data.Monoid
import Network.URI
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)


data GeoGebraEmbed = GeoGebraEmbed
  -- String Properties
  { gg_id          :: Maybe Letters_Numbers
  , gg_class       :: Maybe Css_Class_Name
  , gg_height      :: Maybe Natural_Number_Base_10
  , gg_width       :: Maybe Natural_Number_Base_10
  , gg_bordercolor :: Maybe Hex_Color_Code

  -- Yes/No Properties
  , gg_inputbar    :: Maybe YesNo
  , gg_stylebar    :: Maybe YesNo
  , gg_menubar     :: Maybe YesNo
  , gg_toolbar     :: Maybe YesNo
  , gg_toolbarhelp :: Maybe YesNo
  , gg_reseticon   :: Maybe YesNo
  , gg_clicktoload :: Maybe YesNo
  , gg_rightclick  :: Maybe YesNo
  , gg_labeldrag   :: Maybe YesNo
  , gg_panzoom     :: Maybe YesNo
  } deriving Show


expandGeoGebraShortcodes :: String -> String
expandGeoGebraShortcodes =
  expandShortcodes (emptycode :: GeoGebraEmbed)


-- Constructs the embed URI of a GeoGebraEmbed.
embedUri :: GeoGebraEmbed -> H.AttributeValue
embedUri GeoGebraEmbed{..} = H.stringValue
  $ buildURL HTTPS "www.geogebra.org" path [] []
  where
    path = concat
      [ ["material"]
      , ["iframe"]
      , pathValidPre "id"     gg_id
      , pathValidPre "width"  gg_width
      , pathValidPre "height" gg_height
      , pathValidPre "border" gg_bordercolor
      , pathYesNoPre "ai"     gg_inputbar    "true" "false"
      , pathYesNoPre "asb"    gg_stylebar    "true" "false"
      , pathYesNoPre "smb"    gg_menubar     "true" "false"
      , pathYesNoPre "stb"    gg_toolbar     "true" "false"
      , pathYesNoPre "stbh"   gg_toolbarhelp "true" "false"
      , pathYesNoPre "sri"    gg_reseticon   "true" "false"
      , pathYesNoPre "ctl"    gg_clicktoload "true" "false"
      , pathYesNoPre "rc"     gg_rightclick  "true" "false"
      , pathYesNoPre "ld"     gg_labeldrag   "true" "false"
      , pathYesNoPre "sdz"    gg_panzoom     "true" "false"
      ]


instance Shortcode GeoGebraEmbed where
  tag = ShortcodeTag "geogebra"


  emptycode = GeoGebraEmbed
    -- String Properties
    { gg_id          = Nothing
    , gg_class       = validateMaybe "geogebra-container"
    , gg_height      = Nothing
    , gg_width       = Nothing
    , gg_bordercolor = Nothing

    -- Yes/No Properties
    , gg_inputbar    = Nothing
    , gg_stylebar    = Nothing
    , gg_menubar     = Nothing
    , gg_toolbar     = Nothing
    , gg_toolbarhelp = Nothing
    , gg_reseticon   = Nothing
    , gg_clicktoload = Nothing
    , gg_rightclick  = Nothing
    , gg_labeldrag   = Nothing
    , gg_panzoom     = Nothing
    }


  embedcode gg@GeoGebraEmbed{..} 
    | gg_id /= Nothing = do
        renderHtml $ do
          H.div H.! (attrValid A.class_ gg_class) $ do
            H.iframe H.! mconcat
              [ attrValid A.height gg_height
              , attrValid A.width gg_width
              , A.src $ embedUri gg
              ] $ mempty

    | otherwise = missingError "geogebra" "id"


  attributes =
    -- String Properties
    [ Valid "id"    $ \x gg -> gg { gg_id          = Just x }
    , Valid "color" $ \x gg -> gg { gg_bordercolor = Just x }

    -- Yes/No Properties
    , YesNo "show-input-bar"    $ \x gg -> gg { gg_inputbar    = Just x }
    , YesNo "show-style-bar"    $ \x gg -> gg { gg_stylebar    = Just x }
    , YesNo "show-menu-bar"     $ \x gg -> gg { gg_menubar     = Just x }
    , YesNo "show-tool-bar"     $ \x gg -> gg { gg_toolbar     = Just x }
    , YesNo "show-tool-help"    $ \x gg -> gg { gg_toolbarhelp = Just x }
    , YesNo "show-reset-icon"   $ \x gg -> gg { gg_reseticon   = Just x }
    , YesNo "click-to-load"     $ \x gg -> gg { gg_clicktoload = Just x }
    , YesNo "allow-right-click" $ \x gg -> gg { gg_rightclick  = Just x }
    , YesNo "drag-labels"       $ \x gg -> gg { gg_labeldrag   = Just x }
    , YesNo "allow-pan-zoom"    $ \x gg -> gg { gg_panzoom     = Just x }
    ]
