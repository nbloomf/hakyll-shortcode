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
  , yt_showannot  :: Maybe ShowAnnotations
  , yt_enablejs   :: Maybe EnableJSAPI
  , yt_loop       :: Maybe Loop
  , yt_origin     :: Maybe ()
  , yt_listtype   :: Maybe ListType
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


{- iv_load_policy -}

data ShowAnnotations
  = ShowAnnotationsYes
  | ShowAnnotationsNo
  deriving (Eq, Show)

instance Render ShowAnnotations where
  render ShowAnnotationsYes = "iv_load_policy=1"
  render ShowAnnotationsNo  = "iv_load_policy=3"


{- enablejsapi -}

data EnableJSAPI
  = EnableJSAPIYes
  | EnableJSAPINo
  deriving (Eq, Show)

instance Render EnableJSAPI where
  render EnableJSAPIYes = "enablejsapi=1"
  render EnableJSAPINo  = "enablejsapi=0"


{- loop -}

data Loop
  = LoopYes
  | LoopNo
  deriving (Eq, Show)

instance Render Loop where
  render LoopYes = "loop=1"
  render LoopNo  = "loop=0"


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
          , renderMaybe yt_showannot
          , renderMaybe yt_enablejs
          , renderMaybe yt_loop
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
    , yt_related    = Just ShowRelatedNo
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
    | yt_enablejs /= Just EnableJSAPIYes && yt_origin == Nothing =
        "(Warning: if you set 'enablejs' to 'yes', you should also set 'origin' to your domain.)"

    {- id -}
    | yt_id /= Nothing = do
        let Just yt_id' = yt_id
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
    , OneOf "show-info"
        [ ("yes", \yt -> yt { yt_showinfo = Just ShowInfoYes })
        , ("no",  \yt -> yt { yt_showinfo = Just ShowInfoNo  })
        ]
    , OneOf "show-annotations"
        [ ("yes", \yt -> yt { yt_showannot = Just ShowAnnotationsYes })
        , ("no",  \yt -> yt { yt_showannot = Just ShowAnnotationsNo  })
        ]
    , OneOf "enable-js-api"
        [ ("yes", \yt -> yt { yt_enablejs = Just EnableJSAPIYes })
        , ("no",  \yt -> yt { yt_enablejs = Just EnableJSAPINo  })
        ]
    , OneOf "loop"
        [ ("yes", \yt -> yt { yt_loop = Just LoopYes })
        , ("no",  \yt -> yt { yt_loop = Just LoopNo  })
        ]
    , OneOf "list-type"
        [ ("playlist",     \yt -> yt { yt_listtype = Just ListTypePlaylist    })
        , ("search",       \yt -> yt { yt_listtype = Just ListTypeSearch      })
        , ("user-uploads", \yt -> yt { yt_listtype = Just ListTypeUserUploads })
        ]
    ]
