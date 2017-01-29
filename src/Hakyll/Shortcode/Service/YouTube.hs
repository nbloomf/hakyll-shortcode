{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hakyll.Shortcode.Service.YouTube (
  expandYouTubeShortcodes
) where


import Hakyll.Shortcode.Service
import Hakyll.Shortcode.Render
import Hakyll.Shortcode.Types

import Data.Monoid
import Network.URI
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)



{---------}
{- Types -}
{---------}

-- | A type representing embedded YouTube videos and playlists.
-- See the <https://developers.google.com/youtube/player_parameters API documentation>.
data YouTubeEmbed = YouTubeEmbed
  -- String Properties
  { yt_id         :: Maybe Letters_Numbers_Hyphens_Underscores
  , yt_class      :: Maybe Css_Class_Name
  , yt_height     :: Maybe Natural_Number_Base_10
  , yt_width      :: Maybe Natural_Number_Base_10
  , yt_end        :: Maybe Natural_Number_Base_10
  , yt_start      :: Maybe Natural_Number_Base_10
  , yt_language   :: Maybe Iso_639_1_Language_Code
  , yt_playlist   :: Maybe RFC_3986_Unreserved_Uri_Characters
  , yt_origin     :: Maybe Domain_With_Scheme

  -- Yes/No Properties
  , yt_autoplay   :: Maybe YesNo
  , yt_disablekb  :: Maybe YesNo
  , yt_enablejs   :: Maybe YesNo
  , yt_fullscreen :: Maybe YesNo
  , yt_loop       :: Maybe YesNo
  , yt_playinline :: Maybe YesNo
  , yt_related    :: Maybe YesNo
  , yt_showannot  :: Maybe YesNo
  , yt_showinfo   :: Maybe YesNo
  , yt_showlogo   :: Maybe YesNo

  -- Enumerated Properties
  , yt_captions   :: Maybe CaptionPolicy
  , yt_color      :: Maybe Color
  , yt_controls   :: Maybe ShowControls
  , yt_listtype   :: Maybe ListType
  } deriving Show


{- cc_load_policy -}

data CaptionPolicy
  = ShowCaptions
  deriving Eq

instance Show CaptionPolicy where
  show ShowCaptions = "cc_load_policy=1"


{- controls -}

data ShowControls
  = ShowControlsNever
  | ShowControlsOnload
  | ShowControlsOnplay
  deriving Eq

instance Show ShowControls where
  show ShowControlsNever  = "controls=0"
  show ShowControlsOnload = "controls=1"
  show ShowControlsOnplay = "controls=2"


{- color -}

data Color
  = Red
  | White
  deriving Eq

instance Show Color where
  show Red   = "color=red"
  show White = "color=white"


{- listType -}

data ListType
  = ListTypePlaylist
  | ListTypeSearch
  | ListTypeUserUploads
  deriving Eq

instance Show ListType where
  show ListTypePlaylist    = "listType=playlist"
  show ListTypeSearch      = "listType=search"
  show ListTypeUserUploads = "listType=user_uploads"



{----------------------}
{- Shortcode Instance -}
{----------------------}

-- | Find and replace @youtube@ shortcodes.
expandYouTubeShortcodes :: String -> String
expandYouTubeShortcodes =
  expandShortcodes (emptycode :: YouTubeEmbed)


-- Constructs the embed URI of a YouTubeEmbed.
embedUri :: YouTubeEmbed -> H.AttributeValue
embedUri YouTubeEmbed{..} = H.stringValue
  $ buildURL HTTPS "www.youtube.com" path query []
  where
    path =
      [ "embed"
      , pathValid yt_id
      ]

    query =
      -- String Properties
      [ queryValid yt_start    "start"
      , queryValid yt_end      "end"
      , queryValid yt_language "hl"
      , queryValid yt_playlist "playlist"
      , queryValid yt_origin   "origin"

      -- Yes/No Properties
      , queryYesNo yt_autoplay   "autoplay=1"       "autoplay=0"
      , queryYesNo yt_disablekb  "disablekb=1"      "disablekb=0"
      , queryYesNo yt_enablejs   "enablejsapi=1"    "enablejsapi=0"
      , queryYesNo yt_fullscreen "fs=1"             "fs=0"
      , queryYesNo yt_loop       "loop=1"           "loop=0"
      , queryYesNo yt_playinline "playsinline=1"    "playsinline=0"
      , queryYesNo yt_related    "rel=1"            "rel=0"
      , queryYesNo yt_showannot  "iv_load_policy=1" "iv_load_policy=3"
      , queryYesNo yt_showinfo   "showinfo=1"       "showinfo=0"
      , queryYesNo yt_showlogo   "modestbranding=0" "modestbranding=1"

      -- Enumerated Properties
      , queryOneOf yt_captions
      , queryOneOf yt_color
      , queryOneOf yt_controls
      , queryOneOf yt_listtype
      ]



instance Shortcode YouTubeEmbed where
  tag = ShortcodeTag "youtube"


  emptycode = YouTubeEmbed
    { yt_id         = Nothing
    , yt_class      = validateMaybe "youtube-container"
    , yt_height     = Nothing
    , yt_width      = Nothing
    , yt_autoplay   = Nothing
    , yt_captions   = Nothing
    , yt_controls   = Nothing
    , yt_color      = Nothing
    , yt_disablekb  = Nothing
    , yt_end        = Nothing
    , yt_fullscreen = Nothing
    , yt_related    = Just No
    , yt_start      = Nothing
    , yt_showlogo   = Nothing
    , yt_language   = Nothing
    , yt_playinline = Nothing
    , yt_playlist   = Nothing
    , yt_showinfo   = Nothing
    , yt_showannot  = Nothing
    , yt_enablejs   = Nothing
    , yt_loop       = Nothing
    , yt_origin     = Nothing
    , yt_listtype   = Nothing
    }


  embedcode yt@YouTubeEmbed{..}
    {- check that 'origin' is set if 'enablejs' is 'yes' -}
    | yt_enablejs == Just Yes && yt_origin /= Nothing =
        "(Warning: if you set 'enablejs' to 'yes', you should also set 'origin' to your domain.)"

    {- id or list+listType -}
    | yt_id /= Nothing || (yt_playlist /= Nothing && yt_listtype /= Nothing) = do
        renderHtml $ do
          H.div H.! (attrValid A.class_ yt_class) $ do
            H.iframe H.! mconcat
              [ attrValid A.height yt_height
              , attrValid A.width yt_width
              , A.type_ "text/html"
              , A.src $ embedUri yt
              ] $ mempty

    | otherwise =
        "(Error: either the 'id' or the 'list' and 'list-type' parameter must be set.)"


  attributes =
    -- String Properties
    [ Valid "id"     $ \x yt -> yt { yt_id       = Just x }
    , Valid "class"  $ \x yt -> yt { yt_class    = Just x }
    , Valid "height" $ \x yt -> yt { yt_height   = Just x }
    , Valid "width"  $ \x yt -> yt { yt_width    = Just x }
    , Valid "end"    $ \x yt -> yt { yt_end      = Just x }
    , Valid "start"  $ \x yt -> yt { yt_start    = Just x }
    , Valid "list"   $ \x yt -> yt { yt_playlist = Just x }
    , Valid "origin" $ \x yt -> yt { yt_origin   = Just x }

    -- Yes/No Properties
    , YesNo "loop"             $ \x yt -> yt { yt_loop       = Just x }
    , YesNo "show-related"     $ \x yt -> yt { yt_related    = Just x }
    , YesNo "disable-keyboard" $ \x yt -> yt { yt_disablekb  = Just x }
    , YesNo "autoplay"         $ \x yt -> yt { yt_autoplay   = Just x }
    , YesNo "show-fullscreen"  $ \x yt -> yt { yt_fullscreen = Just x }
    , YesNo "show-info"        $ \x yt -> yt { yt_showinfo   = Just x }
    , YesNo "play-inline"      $ \x yt -> yt { yt_playinline = Just x }
    , YesNo "show-logo"        $ \x yt -> yt { yt_showlogo   = Just x }
    , YesNo "show-annotations" $ \x yt -> yt { yt_showannot  = Just x }
    , YesNo "enable-js-api"    $ \x yt -> yt { yt_enablejs   = Just x }

    -- Enumerated Properties
    , OneOf "captions"
        [ ("show",    \yt -> yt { yt_captions = Just ShowCaptions })
        , ("default", \yt -> yt { yt_captions = Nothing           })
        ]
    , OneOf "show-controls"
        [ ("never",  \yt -> yt { yt_controls = Just ShowControlsNever  })
        , ("onload", \yt -> yt { yt_controls = Just ShowControlsOnload })
        , ("onplay", \yt -> yt { yt_controls = Just ShowControlsOnplay })
        ]
    , OneOf "color"
        [ ("red",   \yt -> yt { yt_color = Just Red   })
        , ("white", \yt -> yt { yt_color = Just White })
        ]
    , OneOf "list-type"
        [ ("playlist",     \yt -> yt { yt_listtype = Just ListTypePlaylist    })
        , ("search",       \yt -> yt { yt_listtype = Just ListTypeSearch      })
        , ("user-uploads", \yt -> yt { yt_listtype = Just ListTypeUserUploads })
        ]
    ]
