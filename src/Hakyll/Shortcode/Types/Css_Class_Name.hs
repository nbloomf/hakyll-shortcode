module Hakyll.Shortcode.Types.Css_Class_Name (
  Css_Class_Name()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


{-------------------}
{- CSS Class Names -}
{-------------------}

newtype Css_Class_Name
  = Make { unMake :: String } deriving Eq

instance Validate Css_Class_Name where
  validate text = case text =~ "^[a-zA-Z][_a-zA-Z0-9-]*$" of
    True  -> Right $ Make text
    False -> Left "Must be a letter, followed by zero or more alphanumeric characters, hyphens, or underscores."

instance Show Css_Class_Name where
  show = unMake
