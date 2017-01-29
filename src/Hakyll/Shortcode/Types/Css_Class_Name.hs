{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.Css_Class_Name (
  Css_Class_Name()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


{-------------------}
{- CSS Class Names -}
{-------------------}

-- | Valid CSS class names: alphanumeric, hyphen, or underscore, but the first character must be alphanumeric.
-- Note: this type represents only a subset of all valid class names.
-- See the <https://www.w3.org/TR/CSS21/grammar.html#scanner CSS grammar>.
newtype Css_Class_Name
  = Make { unMake :: String } deriving Eq

instance Validate Css_Class_Name where
  validate text = case text =~ "^[a-zA-Z][_a-zA-Z0-9-]*$" of
    True  -> Right $ Make text
    False -> Left "Must be a letter, followed by zero or more alphanumeric characters, hyphens, or underscores."

instance Show Css_Class_Name where
  show = unMake
