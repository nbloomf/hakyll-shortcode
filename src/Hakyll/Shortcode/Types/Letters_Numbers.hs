{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.Letters_Numbers (
  Letters_Numbers()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


-- | Strings consisting only of alphanumeric characters.
-- Must be constructed using 'validate'.
newtype Letters_Numbers
  = Make { unMake :: String } deriving Eq


instance Validate Letters_Numbers where
  validate text = case text =~ "^[a-zA-Z0-9]+$" of
    True  -> Right $ Make text
    False -> Left "Must be one or more alphanumeric characters."


instance Show Letters_Numbers where
  show = unMake
