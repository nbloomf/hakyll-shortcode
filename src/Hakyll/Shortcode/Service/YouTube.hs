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
  { yt_id         :: Maybe String_AlphanumericHyphenUnderscore
  , yt_class      :: Maybe String_CSSClassName
  , yt_height     :: Maybe String_DecimalDigits
  , yt_width      :: Maybe String_DecimalDigits
  , yt_autoplay   :: Maybe Autoplay
  , yt_captions   :: Maybe CaptionPolicy
  , yt_related    :: Maybe ShowRelated
  , yt_controls   :: Maybe ShowControls
  , yt_color      :: Maybe Color
  , yt_disablekb  :: Maybe DisableKeyboard
  , yt_end        :: Maybe String_DecimalDigits
  , yt_fullscreen :: Maybe ShowFullscreen
  , yt_start      :: Maybe String_DecimalDigits
  , yt_showlogo   :: Maybe ShowLogo
  , yt_language   :: Maybe String_ISO_639_1
  , yt_playinline :: Maybe PlayInline
  , yt_playlist   :: Maybe String_AlphanumericHyphenUnderscoreComma
  , yt_showinfo   :: Maybe ShowInfo
  } deriving Show


{- autoplay -}

data Autoplay
  = AutoplayYes
  | AutoplayNo
  deriving (Eq, Show)

instance Render Autoplay where
  render AutoplayYes = "autoplay=1"
  render AutoplayNo  = "autoplay=0"


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


{- rel -}

data ShowRelated
  = ShowRelatedYes
  | ShowRelatedNo
  deriving (Eq, Show)

instance Render ShowRelated where
  render ShowRelatedYes = "rel=1"
  render ShowRelatedNo  = "rel=0"


{- color -}

data Color
  = Red
  | White
  deriving (Eq, Show)

instance Render Color where
  render Red   = "color=red"
  render White = "color=white"


{- disablekb -}

data DisableKeyboard
  = DisableKeyboardYes
  | DisableKeyboardNo
  deriving (Eq, Show)

instance Render DisableKeyboard where
  render DisableKeyboardYes = "disablekb=1"
  render DisableKeyboardNo  = "disablekb=0"


{- fs -}

data ShowFullscreen
  = ShowFullscreenYes
  | ShowFullscreenNo
  deriving (Eq, Show)

instance Render ShowFullscreen where
  render ShowFullscreenYes = "fs=1"
  render ShowFullscreenNo  = "fs=0"


{- modestbranding -}

data ShowLogo
  = ShowLogoYes
  | ShowLogoNo
  deriving (Eq, Show)

instance Render ShowLogo where
  render ShowLogoYes = "modestbranding=0"
  render ShowLogoNo  = "modestbranding=1"


{- playsinline -}

data PlayInline
  = PlayInlineYes
  | PlayInlineNo
  deriving (Eq, Show)

instance Render PlayInline where
  render PlayInlineYes = "playsinline=1"
  render PlayInlineNo  = "playsinline=0"


{- showinfo -}

data ShowInfo
  = ShowInfoYes
  | ShowInfoNo
  deriving (Eq, Show)

instance Render ShowInfo where
  render ShowInfoYes = "showinfo=1"
  render ShowInfoNo  = "showinfo=0"



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
          [ renderMaybe yt_autoplay
          , renderMaybe yt_captions
          , renderMaybe yt_related
          , renderMaybe yt_controls
          , renderMaybe yt_color
          , renderMaybe yt_disablekb
          , renderMaybe yt_fullscreen
          , renderMaybe yt_showlogo
          , renderMaybe yt_playinline
          , renderMaybe yt_showinfo
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
    , yt_related    = Just ShowRelatedNo
    , yt_start      = Nothing
    , yt_showlogo   = Nothing
    , yt_language   = Nothing
    , yt_playinline = Nothing
    , yt_playlist   = Nothing
    , yt_showinfo   = Nothing
    }


  embedcode yt@YouTubeEmbed{..} = case yt_id of
    Nothing -> missingError "youtube" "id"
    Just yt_id' -> renderHtml $ do
      H.div H.! (perhaps A.class_ yt_class) $ do
        H.iframe H.! mconcat
          [ perhaps A.height yt_height
          , perhaps A.width yt_width
          , A.type_ "text/html"
          , A.src $ embedUri yt
          ] $ mempty


  attributes =
    [ Valid "id"     $ \x yt -> yt { yt_id     = Just x }
    , Valid "class"  $ \x yt -> yt { yt_class  = Just x }
    , Valid "height" $ \x yt -> yt { yt_height = Just x }
    , Valid "width"  $ \x yt -> yt { yt_width  = Just x }
    , Valid "end"    $ \x yt -> yt { yt_end    = Just x }
    , Valid "start"  $ \x yt -> yt { yt_start  = Just x }

    , OneOf "autoplay"
        [ ("yes", \yt -> yt { yt_autoplay = Just AutoplayYes })
        , ("no",  \yt -> yt { yt_autoplay = Just AutoplayNo  })
        ]
    , OneOf "captions"
        [ ("show",    \yt -> yt { yt_captions = Just ShowCaptions })
        , ("default", \yt -> yt { yt_captions = Nothing           })
        ]
    , OneOf "show-related"
        [ ("yes", \yt -> yt { yt_related = Just ShowRelatedYes })
        , ("no",  \yt -> yt { yt_related = Just ShowRelatedNo  })
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
    , OneOf "disable-keyboard"
        [ ("yes", \yt -> yt { yt_disablekb = Just DisableKeyboardYes })
        , ("no",  \yt -> yt { yt_disablekb = Just DisableKeyboardNo  })
        ]
    , OneOf "show-fullscreen"
        [ ("yes", \yt -> yt { yt_fullscreen = Just ShowFullscreenYes })
        , ("no",  \yt -> yt { yt_fullscreen = Just ShowFullscreenNo  })
        ]
    , OneOf "show-logo"
        [ ("yes", \yt -> yt { yt_showlogo = Just ShowLogoYes })
        , ("no",  \yt -> yt { yt_showlogo = Just ShowLogoNo  })
        ]
    , OneOf "play-inline"
        [ ("yes", \yt -> yt { yt_playinline = Just PlayInlineYes })
        , ("no",  \yt -> yt { yt_playinline = Just PlayInlineNo  })
        ]
    ]
