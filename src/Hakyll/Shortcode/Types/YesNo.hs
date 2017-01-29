{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.YesNo (
  YesNo(Yes,No)
) where


-- | Type representing Yes or No options for a parameter.
-- A slightly more semantically meaningful version of 'Bool'.
data YesNo
  -- | The Yes option.
  = Yes
  -- | The No option.
  | No
  deriving (Eq, Show)
