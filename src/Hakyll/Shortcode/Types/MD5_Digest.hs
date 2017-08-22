{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.MD5_Digest (
  MD5_Digest()
) where

import Hakyll.Shortcode.Validate


-- | Strings of 64 hexadecimal digits. Case insensitive.
newtype MD5_Digest
  = Make { unMake :: String } deriving Eq

instance Validate MD5_Digest where
  validate text =
    let hex_digits = "0123456789abcdefABCDEF" in
    if (length text == 32) && (all (`elem` hex_digits) text) 
      then Right $ Make text
      else Left "MD5 digest must be 32 hexadecimal digits."

instance Show MD5_Digest where
  show = unMake

