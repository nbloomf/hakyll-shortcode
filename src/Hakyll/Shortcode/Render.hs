module Hakyll.Shortcode.Render where

class Render t where
  render :: t -> String

renderMaybe :: (Render t) => Maybe t -> String
renderMaybe Nothing  = ""
renderMaybe (Just x) = render x

renderKeyValMaybe :: (Render t) => String -> Maybe t -> String
renderKeyValMaybe _   Nothing  = ""
renderKeyValMaybe key (Just x) = key ++ "='" ++ render x ++ "'"
