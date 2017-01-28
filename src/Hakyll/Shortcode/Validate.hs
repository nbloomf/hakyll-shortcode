module Hakyll.Shortcode.Validate (
  Validate,
  validate,
  validateMaybe
) where



-- The validate function acts like a safe constructor,
-- allowing us to encode properties of strings as types.

class Validate t where
  validate :: String -> Either String t


-- Throw away the error message.
validateMaybe :: (Validate t) => String -> Maybe t
validateMaybe x = case validate x of
  Right y -> Just y
  Left _  -> Nothing
