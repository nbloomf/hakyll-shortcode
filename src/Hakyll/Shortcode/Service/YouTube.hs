{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hakyll.Shortcode.Service.YouTube (
  expandYouTubeShortcodes
) where


import Hakyll.Shortcode.Service
import Hakyll.Shortcode.Render
import Hakyll.Shortcode.Validate
import Hakyll.Shortcode.Html

import Data.Monoid
import Data.List (intercalate)
import Network.URI
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)



{---------}
{- Types -}
{---------}

data YouTubeEmbed = YouTubeEmbed
  -- String Properties
  { yt_id         :: Maybe String_AlphanumericHyphenUnderscore
  , yt_class      :: Maybe String_CSSClassName
  , yt_height     :: Maybe String_DecimalDigits
  , yt_width      :: Maybe String_DecimalDigits
  , yt_end        :: Maybe String_DecimalDigits
  , yt_start      :: Maybe String_DecimalDigits
  , yt_language   :: Maybe String_ISO_639_1
  , yt_playlist   :: Maybe String_AlphanumericHyphenUnderscoreComma
  , yt_origin     :: Maybe ()

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
  deriving (Eq, Show)

instance Render CaptionPolicy where
  render ShowCaptions = "cc_load_policy=1"


{- controls -}

data ShowControls
  = ShowControlsNever
  | ShowControlsOnload
  | ShowControlsOnplay
  deriving (Eq, Show)

instance Render ShowControls where
  render ShowControlsNever  = "controls=0"
  render ShowControlsOnload = "controls=1"
  render ShowControlsOnplay = "controls=2"


{- color -}

data Color
  = Red
  | White
  deriving (Eq, Show)

instance Render Color where
  render Red   = "color=red"
  render White = "color=white"


{- listType -}

data ListType
  = ListTypePlaylist
  | ListTypeSearch
  | ListTypeUserUploads
  deriving (Eq, Show)

instance Render ListType where
  render ListTypePlaylist    = "listType=playlist"
  render ListTypeSearch      = "listType=search"
  render ListTypeUserUploads = "listType=user_uploads"



{----------------------}
{- Shortcode Instance -}
{----------------------}

expandYouTubeShortcodes :: String -> String
expandYouTubeShortcodes =
  expandShortcodes (emptycode :: YouTubeEmbed)


-- Constructs the embed URI of a YouTubeEmbed.
embedUri :: YouTubeEmbed -> H.AttributeValue
embedUri YouTubeEmbed{..} = H.stringValue $ uriToString show uri ""
  where
    uri = URI
      { uriScheme = "https:"
      , uriAuthority = Just $ URIAuth
          { uriUserInfo = ""
          , uriRegName  = "www.youtube.com"
          , uriPort     = ""
          }
      , uriPath     = "/embed" ++ yt_id'
      , uriQuery    = query
      , uriFragment = ""
      }

    yt_id' = case yt_id of
      Nothing -> ""
      Just x  -> '/' : render x

    query =
      let
        str = queryAmpSep
          [ ifYesNo yt_autoplay   "autoplay=1"       "autoplay=0"
          , ifYesNo yt_disablekb  "disablekb=1"      "disablekb=0"
          , ifYesNo yt_enablejs   "enablejsapi=1"    "enablejsapi=0"
          , ifYesNo yt_fullscreen "fs=1"             "fs=0"
          , ifYesNo yt_loop       "loop=1"           "loop=0"
          , ifYesNo yt_playinline "playsinline=1"    "playsinline=0"
          , ifYesNo yt_related    "rel=1"            "rel=0"
          , ifYesNo yt_showannot  "iv_load_policy=1" "iv_load_policy=3"
          , ifYesNo yt_showinfo   "showinfo=1"       "showinfo=0"
          , ifYesNo yt_showlogo   "modestbranding=0" "modestbranding=1"
          , renderMaybe yt_captions
          , renderMaybe yt_controls
          , renderMaybe yt_color
          , renderMaybe yt_listtype
          , renderKeyValMaybe "start"    yt_start
          , renderKeyValMaybe "end"      yt_end
          , renderKeyValMaybe "hl"       yt_language
          , renderKeyValMaybe "playlist" yt_playlist
          ]
      in
        (if null str then "" else "?") ++ str


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

    {- id -}
    | yt_id /= Nothing = do
        renderHtml $ do
          H.div H.! (perhaps A.class_ yt_class) $ do
            H.iframe H.! mconcat
              [ perhaps A.height yt_height
              , perhaps A.width yt_width
              , A.type_ "text/html"
              , A.src $ embedUri yt
              ] $ mempty

    | otherwise =
        "(Error: either the 'id' or the 'list' and 'list-type' parameter must be set.)"


  attributes =
    -- String Properties
    [ Valid "id"     $ \x yt -> yt { yt_id     = Just x }
    , Valid "class"  $ \x yt -> yt { yt_class  = Just x }
    , Valid "height" $ \x yt -> yt { yt_height = Just x }
    , Valid "width"  $ \x yt -> yt { yt_width  = Just x }
    , Valid "end"    $ \x yt -> yt { yt_end    = Just x }
    , Valid "start"  $ \x yt -> yt { yt_start  = Just x }

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
