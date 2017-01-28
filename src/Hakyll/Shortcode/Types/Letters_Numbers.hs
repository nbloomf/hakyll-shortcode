module Hakyll.Shortcode.Types.Letters_Numbers (
  Letters_Numbers()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


{----------------}
{- Alphanumeric -}
{----------------}

newtype Letters_Numbers
  = Make { unMake :: String } deriving Eq

instance Validate Letters_Numbers where
  validate text = case text =~ "^[a-zA-Z0-9]+$" of
    True  -> Right $ Make text
    False -> Left "Must be one or more alphanumeric characters."

instance Show Letters_Numbers where
  show = unMake
