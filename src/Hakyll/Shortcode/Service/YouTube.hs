{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hakyll.Shortcode.Service.YouTube (
  expandYouTubeShortcodes
) where

import Hakyll.Shortcode.Parser
import Hakyll.Shortcode.Error
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
  render ShowCaptions    = "cc_load_policy=1"


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
        str = queryCommaSep
          [ renderMaybe yt_autoplay
          , renderMaybe yt_captions
          , renderMaybe yt_related
          , renderMaybe yt_controls
          , renderMaybe yt_color
          , renderMaybe yt_disablekb
          , renderMaybe yt_fullscreen
          , renderKeyValMaybe "start" yt_start
          , renderKeyValMaybe "end"   yt_end
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


  update YouTubeEmbed{..} (key,val) = case key of
    "id" -> case validate val of
      Right x -> do
        let yt_id = Just x
        return YouTubeEmbed{..}
      Left msg -> do
        Left $ validateError "youtube" key val msg

    "class" -> case validate val of
      Right x -> do
        let yt_class = Just x
        return YouTubeEmbed{..}
      Left msg -> do
        Left $ validateError "youtube" key val msg

    "height" -> case validate val of
      Right x -> do
        let yt_height = Just x
        return YouTubeEmbed{..}
      Left msg -> do
        Left $ validateError "youtube" key val msg

    "width" -> case validate val of
      Right x -> do
        let yt_width = Just x
        return YouTubeEmbed{..}
      Left msg -> do
        Left $ validateError "youtube" key val msg

    "autoplay" -> case val of
      "yes" -> do
        let yt_autoplay = Just AutoplayYes
        return YouTubeEmbed{..}
      "no" -> do
        let yt_autoplay = Just AutoplayNo
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" key val
             "Expected 'yes' or 'no'."

    "captions" -> case val of
      "show" -> do
        let yt_captions = Just ShowCaptions
        return YouTubeEmbed{..}
      "default" -> do
        let yt_captions = Nothing
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" key val
             "Expected 'show' or 'default'."

    "show-related" -> case val of
      "yes" -> do
        let yt_related = Just ShowRelatedYes
        return YouTubeEmbed{..}
      "no" -> do
        let yt_related = Just ShowRelatedNo
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" key val
             "Expected 'yes' or 'no'."

    "show-controls" -> case val of
      "never" -> do
        let yt_controls = Just ShowControlsNever
        return YouTubeEmbed{..}
      "onload" -> do
        let yt_controls = Just ShowControlsOnload
        return YouTubeEmbed{..}
      "onplay" -> do
        let yt_controls = Just ShowControlsOnplay
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" key val
             "Expected 'never', 'onload', or 'onplay'."

    "color" -> case val of
      "red" -> do
        let yt_color = Just Red
        return YouTubeEmbed{..}
      "onload" -> do
        let yt_color = Just White
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" key val
             "Expected 'red' or 'white'."

    "disable-keyboard" -> case val of
      "yes" -> do
        let yt_disablekb = Just DisableKeyboardYes
        return YouTubeEmbed{..}
      "no" -> do
        let yt_disablekb = Just DisableKeyboardNo
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" key val
             "Expected 'yes' or 'no'."

    "end" -> case validate val of
      Right x -> do
        let yt_end = Just x
        return YouTubeEmbed{..}
      Left msg -> do
        Left $ validateError "youtube" key val msg

    "show-fullscreen" -> case val of
      "yes" -> do
        let yt_fullscreen = Just ShowFullscreenYes
        return YouTubeEmbed{..}
      "no" -> do
        let yt_fullscreen = Just ShowFullscreenNo
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" key val
             "Expected 'yes' or 'no'."

    "start" -> case validate val of
      Right x -> do
        let yt_start = Just x
        return YouTubeEmbed{..}
      Left msg -> do
        Left $ validateError "youtube" key val msg

    otherwise -> return YouTubeEmbed{..}
