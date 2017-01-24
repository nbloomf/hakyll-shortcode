{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hakyll.Shortcode.Validate (
  validate,
  String_DecimalDigits(),
  isAlphanumericHyphenUnderscore,
  msgAlphanumericHyphenUnderscore,
  isValidCSSClassName,
  msgValidCSSClassName
) where

import Text.Regex.Posix

-- The validate function acts like a safe constructor,
-- allowing us to encode properties of strings as types.

class Validate t where
  validate :: String -> Either String t



{------------------}
{- Decimal Digits -}
{------------------}

type String_DecimalDigits = String

instance Validate String_DecimalDigits where
  validate text = case text =~ "^[0-9]+$" of
    True  -> Right text
    False -> Left "Must be one or more decimal digits (0-9)."
  

isAlphanumericHyphenUnderscore :: String -> Bool
isAlphanumericHyphenUnderscore text = 
  text =~ "^[_a-zA-Z0-9-]+$"

msgAlphanumericHyphenUnderscore :: String
msgAlphanumericHyphenUnderscore =
  "Must be one or more alphanumeric characters, hyphens, or underscores."

isValidCSSClassName :: String -> Bool
isValidCSSClassName text =
  text =~ "^[a-zA-Z][_a-zA-Z0-9-]*$"

msgValidCSSClassName :: String
msgValidCSSClassName =
  "Must be a letter, followed by zero or more alphanumeric characters, hyphens, or underscores."

