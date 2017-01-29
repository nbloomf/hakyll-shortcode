{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.Natural_Number_Base_10 (
  Natural_Number_Base_10()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


-- | Either 0, or a nonempty string of decimal digits with no leading zeros.
newtype Natural_Number_Base_10
  = Make { unMake :: String } deriving Eq

instance Validate Natural_Number_Base_10 where
  validate "0"  = Right $ Make "0"
  validate text = case text =~ "^[1-9][0-9]*$" of
    True  -> Right $ Make text
    False -> Left "Must be '0' or one or more decimal digits (0-9) with no leading zero."

instance Show Natural_Number_Base_10 where
  show = unMake
