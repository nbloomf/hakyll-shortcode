{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Hakyll.Shortcode.Validate (
  Validate,
  validate,
  validateMaybe,
  String_DecimalDigits(),
  String_CSSClassName(),
  String_AlphanumericHyphenUnderscore(),
  String_AlphanumericHyphenUnderscoreComma(),
  String_ISO_639_1()
) where

import Text.Regex.Posix

import Hakyll.Shortcode.Render

-- The validate function acts like a safe constructor,
-- allowing us to encode properties of strings as types.

class Validate t where
  validate :: String -> Either String t


-- Throw away the error message.
validateMaybe :: (Validate t) => String -> Maybe t
validateMaybe x = case validate x of
  Right y -> Just y
  Left _  -> Nothing



{------------------}
{- Decimal Digits -}
{------------------}

newtype String_DecimalDigits
  = String_DecimalDigits
     { unString_DecimalDigits :: String
     } deriving (Eq, Show)

instance Validate String_DecimalDigits where
  validate text = case text =~ "^[0-9]+$" of
    True  -> Right $ String_DecimalDigits text
    False -> Left "Must be one or more decimal digits (0-9)."

instance Render String_DecimalDigits where
  render = unString_DecimalDigits



{-------------------}
{- CSS Class Names -}
{-------------------}

newtype String_CSSClassName
  = String_CSSClassName
     { unString_CSSClassName :: String
     } deriving (Eq, Show)

instance Validate String_CSSClassName where
  validate text = case text =~ "^[a-zA-Z][_a-zA-Z0-9-]*$" of
    True  -> Right $ String_CSSClassName text
    False -> Left "Must be a letter, followed by zero or more alphanumeric characters, hyphens, or underscores."

instance Render String_CSSClassName where
  render = unString_CSSClassName



{--------------------------------------}
{- Alphanumeric, Hyphens, Underscores -}
{--------------------------------------}

newtype String_AlphanumericHyphenUnderscore
  = String_AlphanumericHyphenUnderscore
      { unString_AlphanumericHyphenUnderscore :: String
      } deriving (Eq, Show)

instance Validate String_AlphanumericHyphenUnderscore where
  validate text = case text =~ "^[_a-zA-Z0-9-]+$" of
    True  -> Right $ String_AlphanumericHyphenUnderscore text
    False -> Left "Must be one or more alphanumeric characters, hyphens, or underscores."

instance Render String_AlphanumericHyphenUnderscore where
  render = unString_AlphanumericHyphenUnderscore



{----------------------------------------------}
{- Alphanumeric, Hyphens, Underscores, Commas -}
{----------------------------------------------}

newtype String_AlphanumericHyphenUnderscoreComma
  = String_AlphanumericHyphenUnderscoreComma
      { unString_AlphanumericHyphenUnderscoreComma :: String
      } deriving (Eq, Show)

instance Validate String_AlphanumericHyphenUnderscoreComma where
  validate text = case text =~ "^[,_a-zA-Z0-9-]+$" of
    True  -> Right $ String_AlphanumericHyphenUnderscoreComma text
    False -> Left "Must be one or more alphanumeric characters, hyphens, underscores, or commas."

instance Render String_AlphanumericHyphenUnderscoreComma where
  render = unString_AlphanumericHyphenUnderscoreComma



{----------------------------}
{- ISO 639.1 Language Codes -}
{----------------------------}

newtype String_ISO_639_1
  = String_ISO_639_1
      { unString_ISO_639_1 :: String
      } deriving (Eq, Show)

instance Validate String_ISO_639_1 where
  validate text = if elem text codes
    then Right $ String_ISO_639_1 text
    else Left "Must be an ISO639.1 two-letter language code."
    where
      codes =
        [ "ab", "aa", "af", "ak", "sq", "am", "ar", "an", "hy", "as"
        , "av", "ae", "ay", "az", "bm", "ba", "eu", "be", "bn", "bh"
        , "bi", "bs", "br", "bg", "my", "ca", "ch", "ce", "ny", "zh"
        , "cv", "kw", "co", "cr", "hr", "cs", "da", "dv", "nl", "dz"
        , "en", "eo", "et", "ee", "fo", "fj", "fi", "fr", "ff", "gl"
        , "ka", "de", "el", "gn", "gu", "ht", "ha", "he", "hz", "hi"
        , "ho", "hu", "ia", "id", "ie", "ga", "ig", "ik", "io", "is"
        , "it", "iu", "ja", "jv", "kl", "kn", "kr", "ks", "kk", "km"
        , "ki", "rw", "ky", "kv", "kg", "ko", "ku", "kj", "la", "lb"
        , "lg", "li", "ln", "lo", "lt", "lu", "lv", "gv", "mk", "mg"
        , "ms", "ml", "mt", "mi", "mr", "mh", "mn", "na", "nv", "nd"
        , "ne", "ng", "nb", "nn", "no", "ii", "nr", "oc", "oj", "cu"
        , "om", "or", "os", "pa", "pi", "fa", "pl", "ps", "pt", "qu"
        , "rm", "rn", "ro", "ru", "sa", "sc", "sd", "se", "sm", "sg"
        , "sr", "gd", "sn", "si", "sk", "sl", "so", "st", "es", "su"
        , "sw", "ss", "sv", "ta", "te", "tg", "th", "ti", "bo", "tk"
        , "tl", "tn", "to", "tr", "ts", "tt", "tw", "ty", "ug", "uk"
        , "ur", "uz", "ve", "vi", "vo", "wa", "cy", "wo", "fy", "xh"
        , "yi", "yo", "za", "zu"
        ]

instance Render String_ISO_639_1 where
  render = unString_ISO_639_1
