module Hakyll.Shortcode.Types.YesNo (
  YesNo(Yes,No)
) where


data YesNo = Yes | No
  deriving (Eq, Show)
