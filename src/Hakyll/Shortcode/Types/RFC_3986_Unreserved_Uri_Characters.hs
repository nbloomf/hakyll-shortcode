{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.RFC_3986_Unreserved_Uri_Characters (
  RFC_3986_Unreserved_Uri_Characters()
) where


import Text.Regex.Posix ((=~))

import Hakyll.Shortcode.Validate


-- | Strings consisting only of a-z, A-Z, 0-9, -, _, ., or ~ characters.
-- See [RFC 3986, Section 2.3](https://tools.ietf.org/html/rfc3986#section-2.3).
-- Must be constructed using 'validate'.
newtype RFC_3986_Unreserved_Uri_Characters
  = Make { unMake :: String } deriving Eq


instance Validate RFC_3986_Unreserved_Uri_Characters where
  validate text = case text =~ "^[-a-zA-Z0-9_\\.~]+$" of
    True  -> Right $ Make text
    False -> Left "Must be one or more of a-z, A-Z, 0-9, -, ., _, or ~."


instance Show RFC_3986_Unreserved_Uri_Characters where
  show = unMake
