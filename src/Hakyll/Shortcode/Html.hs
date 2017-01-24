module Hakyll.Shortcode.Html where

import Data.Monoid
import Data.List (intercalate)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Renderer.String ()
import Network.URI


-- 
perhaps :: (Monoid a) => (H.AttributeValue -> a) -> Maybe String -> a
perhaps key Nothing  = mempty
perhaps key (Just x) = key $ H.stringValue $ x


-- Escape all reserved characters except '='.
queryCommaSep :: [String] -> String
queryCommaSep = intercalate "," . filter (/= "") . map (escapeURIString encode)
  where
    encode c = not $ (isReserved c) && (c /= '=')
