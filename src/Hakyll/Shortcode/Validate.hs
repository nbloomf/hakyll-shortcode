{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hakyll.Shortcode.Validate (
  validate,
  validateMaybe,
  String_DecimalDigits(),
  String_CSSClassName(),
  String_AlphanumericHyphenUnderscore()
) where

import Text.Regex.Posix

import Hakyll.Shortcode.Render

-- The validate function acts like a safe constructor,
-- allowing us to encode properties of strings as types.

class Validate t where
  validate :: String -> Either String t


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
