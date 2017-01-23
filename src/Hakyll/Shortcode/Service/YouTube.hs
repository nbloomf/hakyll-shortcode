{-# LANGUAGE RecordWildCards #-}

module Hakyll.Shortcode.Service.YouTube (
  expandYouTubeShortcodes
) where

import Hakyll.Shortcode.Parser
import Hakyll.Shortcode.Error
import Hakyll.Shortcode.Render
import Hakyll.Shortcode.Validate
import Data.List (intercalate)



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
    , yt_class    = Nothing
    , yt_height   = Nothing
    , yt_width    = Nothing
    , yt_autoplay = Nothing
    , yt_captions = Nothing
    , yt_related  = Nothing
    , yt_controls = Nothing
    }


  embedcode YouTubeEmbed{..} =
    let
      attributes = concat
        [ " type='text/html'"
        , showAttribute "height" yt_height
        , showAttribute "width" yt_width
        , " frameborder='0'"
        ]

      query_uri =
        let
          fragment = intercalate "," $ filter (/= "")
            [ renderMaybe yt_autoplay
            , renderMaybe yt_captions
            , renderMaybe yt_related
            , renderMaybe yt_controls
            ]
        in
          if fragment == ""
            then ""
            else '?' : fragment

    in
      case yt_id of
        Nothing -> missingError "youtube" "id"
        Just yt_id' -> concat
          [ "<div" ++ showAttribute "class" yt_class ++ ">"
          , "<iframe"
          , attributes
          , " src='https://www.youtube.com/embed/" ++ yt_id' ++ query_uri ++ "'"
          , "></iframe></div>"
          ]
