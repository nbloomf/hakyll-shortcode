{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.Letters_Numbers_Hyphens_Underscores (
  Letters_Numbers_Hyphens_Underscores()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


-- | Strings consisting only of a-z, A-Z, 0-9, -, and _.
newtype Letters_Numbers_Hyphens_Underscores
  = Make { unMake :: String } deriving Eq

instance Validate Letters_Numbers_Hyphens_Underscores where
  validate text = case text =~ "^[_a-zA-Z0-9-]+$" of
    True  -> Right $ Make text
    False -> Left "Must be one or more alphanumeric characters, hyphens, or underscores."

instance Show Letters_Numbers_Hyphens_Underscores where
  show = unMake
