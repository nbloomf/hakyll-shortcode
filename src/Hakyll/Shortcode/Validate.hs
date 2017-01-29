{-|
Copyright  : (c) Nathan Bloomfield, 2017
License    : GPL-3
Maintainer : nbloomf@gmail.com
Stability  : experimental
-}

module Hakyll.Shortcode.Validate (
  Validate(..),
  validateMaybe
) where


-- | The 'Validate' class lets us approximate subtypes
-- of 'String'. Instances should not expose a constructor.
class Validate t where
  -- | 'validate' acts as a safe constructor.
  validate :: String -> Either String t


-- | Apply 'validate', but map the error message to 'Nothing'.
validateMaybe :: (Validate t) => String -> Maybe t
validateMaybe x = case validate x of
  Right y -> Just y
  Left _  -> Nothing
