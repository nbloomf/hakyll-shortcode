module Hakyll.Shortcode.Types.Domain_With_Scheme (
  Domain_With_Scheme()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


newtype Domain_With_Scheme
  = Make { unMake :: String } deriving Eq

instance Validate Domain_With_Scheme where
  validate text = case text =~ "^[a-z]+://[\\.a-zA-Z-]+$" of
    True  -> Right $ Make text
    False -> Left "Must be a valid domain name with scheme."

instance Show Domain_With_Scheme where
  show = unMake
