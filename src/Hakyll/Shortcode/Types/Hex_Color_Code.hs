module Hakyll.Shortcode.Types.Hex_Color_Code (
  Hex_Color_Code()
) where

import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


{----------------}
{- Alphanumeric -}
{----------------}

newtype Hex_Color_Code
  = Make { unMake :: String } deriving Eq

instance Validate Hex_Color_Code where
  validate text = case text =~ "^[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]$" of
    True  -> Right $ Make text
    False -> Left "Must be six hexadecimal digits."

instance Show Hex_Color_Code where
  show = unMake

