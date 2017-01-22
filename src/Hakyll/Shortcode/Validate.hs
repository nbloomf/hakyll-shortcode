module Hakyll.Shortcode.Validate (
  isDecimalDigits,
  msgDecimalDigits,
  isAlphanumericHyphenUnderscore,
  msgAlphanumericHyphenUnderscore,
  isValidCSSClassName,
  msgValidCSSClassName
) where

import Text.Regex.Posix

isDecimalDigits :: String -> Bool
isDecimalDigits text =
  text =~ "^[0-9]+$"

msgDecimalDigits :: String
msgDecimalDigits =
  "Must be one or more decimal digits (0-9)."

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

