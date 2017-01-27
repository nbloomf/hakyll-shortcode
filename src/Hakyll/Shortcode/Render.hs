module Hakyll.Shortcode.Render where

import Hakyll.Shortcode.YesNo

class Render t where
  render :: t -> String

renderMaybe :: (Render t) => Maybe t -> String
renderMaybe Nothing  = ""
renderMaybe (Just x) = render x

renderKeyValMaybe :: (Render t) => String -> Maybe t -> String
renderKeyValMaybe _   Nothing  = ""
renderKeyValMaybe key (Just x) = key ++ "='" ++ render x ++ "'"

ifYesNo :: Maybe YesNo -> String -> String -> String
ifYesNo x yes no = case x of
  Nothing  -> ""
  Just Yes -> yes
  Just No  -> no
