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
  { yt_id       :: Maybe String
  , yt_class    :: Maybe String
  , yt_height   :: Maybe String
  , yt_width    :: Maybe String
  , yt_autoplay :: Maybe Autoplay
  , yt_captions :: Maybe CaptionPolicy
  , yt_related  :: Maybe ShowRelated
  , yt_controls :: Maybe ShowControls
  } deriving Show


data Autoplay
  = AutoplayYes
  | AutoplayNo
  deriving (Eq, Show)

instance Render Autoplay where
  render AutoplayYes = "autoplay=1"
  render AutoplayNo  = "autoplay=0"


data CaptionPolicy
  = ShowCaptions
  deriving (Eq, Show)

instance Render CaptionPolicy where
  render ShowCaptions    = "cc_load_policy=1"


data ShowControls
  = ShowControlsNever
  | ShowControlsOnload
  | ShowControlsOnplay
  deriving (Eq, Show)

instance Render ShowControls where
  render ShowControlsNever  = "controls=0"
  render ShowControlsOnload = "controls=1"
  render ShowControlsOnplay = "controls=2"


data ShowRelated
  = ShowRelatedYes
  | ShowRelatedNo
  deriving (Eq, Show)

instance Render ShowRelated where
  render ShowRelatedYes = "rel=1"
  render ShowRelatedNo  = "rel=0"


{----------------------}
{- Shortcode Instance -}
{----------------------}

expandYouTubeShortcodes :: String -> String
expandYouTubeShortcodes =
  expandShortcodes (emptycode :: YouTubeEmbed)


instance Shortcode YouTubeEmbed where
  tag = ShortcodeTag "youtube"


  update YouTubeEmbed{..} (key,val) = case key of
    "id" -> do
      if isAlphanumericHyphenUnderscore val
        then do
          let yt_id = Just val
          return YouTubeEmbed{..}
        else do
          Left $ validateError "youtube" "id" val
            msgAlphanumericHyphenUnderscore

    "class" -> do
      if isValidCSSClassName val
        then do
          let yt_class = Just val
          return YouTubeEmbed{..}
        else do
          Left $ validateError "youtube" "class" val
            msgValidCSSClassName

    "height" -> do
      if isDecimalDigits val
        then do
          let yt_height = Just val
          return YouTubeEmbed{..}
        else do
          Left $ validateError "youtube" "height" val
            msgDecimalDigits

    "width" -> do
      if isDecimalDigits val
        then do
          let yt_width = Just val
          return YouTubeEmbed{..}
        else do
          Left $ validateError "youtube" "width" val
            msgDecimalDigits

    "autoplay" -> case val of
      "yes" -> do
        let yt_autoplay = Just AutoplayYes
        return YouTubeEmbed{..}
      "no" -> do
        let yt_autoplay = Just AutoplayNo
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" "autoplay" val
             "Expected 'yes' or 'no'."

    "captions" -> case val of
      "show" -> do
        let yt_captions = Just ShowCaptions
        return YouTubeEmbed{..}
      "default" -> do
        let yt_captions = Nothing
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" "captions" val
             "Expected 'show' or 'default'."

    "show-related" -> case val of
      "yes" -> do
        let yt_related = Just ShowRelatedYes
        return YouTubeEmbed{..}
      "no" -> do
        let yt_related = Just ShowRelatedNo
        return YouTubeEmbed{..}
      _ -> Left $ typeError "youtube" "show-related" val
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
      _ -> Left $ typeError "youtube" "show-related" val
             "Expected 'never', 'onload', or 'onplay'."

    otherwise -> return YouTubeEmbed{..}


  emptycode = YouTubeEmbed
    { yt_id       = Nothing
    , yt_class    = Just "youtube-container"
    , yt_height   = Nothing
    , yt_width    = Nothing
    , yt_autoplay = Nothing
    , yt_captions = Nothing
    , yt_related  = Just ShowRelatedNo
    , yt_controls = Nothing
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
      Just x  -> '/' : x

    query =
      let
        str = queryCommaSep
          [ renderMaybe yt_autoplay
          , renderMaybe yt_captions
          , renderMaybe yt_related
          , renderMaybe yt_controls
          ]
      in
        (if null str then "" else "?") ++ str
