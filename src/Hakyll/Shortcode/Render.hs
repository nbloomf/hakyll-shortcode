module Hakyll.Shortcode.Render where

class Render t where
  render :: t -> String

renderMaybe :: (Render t) => Maybe t -> String
renderMaybe Nothing  = ""
renderMaybe (Just x) = render x

showAttribute :: String -> Maybe String -> String
showAttribute _ Nothing = ""
showAttribute key (Just val) = " " ++ key ++ "='" ++ val ++ "'"
