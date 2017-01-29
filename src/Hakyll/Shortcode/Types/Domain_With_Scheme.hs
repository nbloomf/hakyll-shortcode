{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.Domain_With_Scheme (
  Domain_With_Scheme()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


-- | Matches domain names with the https scheme.
-- The regex for this match is very simple.
newtype Domain_With_Scheme
  = Make { unMake :: String } deriving Eq

instance Validate Domain_With_Scheme where
  validate text = case text =~ "^https://[\\.a-zA-Z-]+$" of
    True  -> Right $ Make text
    False -> Left "Must be a valid domain name with scheme."

instance Show Domain_With_Scheme where
  show = unMake
