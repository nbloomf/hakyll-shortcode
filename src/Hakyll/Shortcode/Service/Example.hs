{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental

Do you want to make more shortcodes? Of course, we all do.

This module demonstrates how to implement a type-safe shortcode.
The basic steps are as follows:

  1. Define a type @t@ which represents the state of a shortcode.
     Typically @t@ will be a record type, and the fields should all
     be Maybes or Lists of instances of the 'Validate' class.

  2. Define a default instance of your shortcode state. This may
     consist of a bunch of @Nothing@s and @[]@s, or maybe you prefer
     nontrivial defaults. This is the 'emptycode' method of the
     'Shortcode' class.

  3. Declare the usable keys for your shortcode by giving a list of
     'ShortcodeAttribute t's. This type lets us define our keys in
     a declarative way, sort of like GetOpt. This is the 'attributes'
     method of the 'Shortcode' class.

  4. Provide a map @t -> String@, to be used to convert abstract
     shortcodes to their expanded form. This is the 'embedcode'
     method of the 'Shortcode' class.

  5. Last but not least, declare which tag will be used to name
     your shortcode.

And that's it! The shortcode API and type library will take care of
parsing, validation, and sanitization for you, giving you a function
@String -> String@ that expands your new shortcode while making sure
only input conforming to your type model is allowed.

Now let's walk through a sample shortcode module.
-}


-- | A couple of language extensions will be useful.
-- The OverloadedStrings extension is used by Text.Blaze
-- to minimize boilerplate. Similarly, RecordWildCards will
-- make working with large records less verbose.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- | We only need to export one function: an alias for
-- the 'expandShortcodes' function. More on this later.
module Hakyll.Shortcode.Service.Example(
  expandExampleShortcodes
) where


-- | Now for some imports; these bring in the Shortcode API.
import Hakyll.Shortcode.Service -- Handles parsing and expanding shortcodes.
import Hakyll.Shortcode.Render  -- Helper functions for rendering URLs and HTML.
import Hakyll.Shortcode.Types   -- A library of validatable string-like types.


-- | These modules are not necessary for defining a shortcode.
-- However, we will use them to more easily construct sanitized HTML.
import Data.Monoid
import Network.URI
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)


-- For the sake of an example, I want my shortcode to look like this:
--
--   [example valid='foo' list='bar' yesno='baz' oneof='qux']
--
-- subject to the following constraints:
--   1. foo should be a sequence of digits,
--   2. bar should be a sequence of letters and numbers;
--   3. baz should be either 'yes' or 'no'; and
--   4. qux should be either 'john', 'paul', 'george', or 'ringo'.
--
-- Moreover,
--   1. I want to require that 'valid' be present;
--   2. I only want one 'yesno' and 'oneof'; and
--   3. I want to be able to give zero or more 'list' values.


-- | We can represent this information using the following type.
-- Note that @Maybe@ means "zero or one" and @[]@ means "zero or more".
data Example = Example
  { ex_valid :: Maybe Decimal_Digits
  , ex_list  :: [Letters_Numbers]
  , ex_yesno :: Maybe YesNo
  , ex_oneof :: Maybe Beatle
  }

data Beatle
  = John
  | Paul
  | George
  | Ringo


-- | Find and replace @example@ shortcodes.
expandGeoGebraShortcodes :: String -> String
expandGeoGebraShortcodes =
  expandShortcodes (emptycode :: Example)


-- | Constructs the embed URI of a GeoGebraEmbed.
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
  tag = ShortcodeTag "example"


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
    [ Valid "valid" $ \x ex -> ex { ex_valid = Just x }
    , Valid "item"  $ \x ex -> ex { ex_list  = Just x }

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

