module Hakyll.Shortcode.Html where

import Data.Monoid
import Data.List (intercalate)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Renderer.String ()
import Network.URI

import Hakyll.Shortcode.Render


-- 
perhaps :: (Monoid a, Render b) => (H.AttributeValue -> a) -> Maybe b -> a
perhaps key Nothing  = mempty
perhaps key (Just x) = key $ H.stringValue $ render x


-- Escape all reserved characters except '='.
queryCommaSep :: [String] -> String
queryCommaSep = intercalate "," . filter (/= "") . map (escapeURIString encode)
  where
    encode c = not $ (isReserved c) && (c /= '=')


-- Escape all reserved characters except '='.
queryAmpSep :: [String] -> String
queryAmpSep = intercalate "&" . filter (/= "") . map (escapeURIString encode)
  where
    encode c = not $ (isReserved c) && (c /= '=')
