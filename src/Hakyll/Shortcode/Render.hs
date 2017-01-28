module Hakyll.Shortcode.Render (
  Scheme(..),
  buildURL,

  queryValid,
  queryOneOf,
  queryYesNo,

  pathValid,
  pathYesNo,
  pathValidPre,
  pathYesNoPre,
  
  attrValid
) where

import Data.List (intercalate)
import Network.URI
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Renderer.String ()
import Data.Monoid

import Hakyll.Shortcode.Types.YesNo


data Scheme
  = HTTPS

instance Show Scheme where
  show HTTPS = "https"


buildURL :: Scheme -> String -> [String] -> [String] -> [String] -> String
buildURL scheme auth path query frag = uriToString show uri ""
  where
    uri = URI
      { uriScheme = show scheme ++ ":"
      , uriAuthority = Just $ URIAuth
          { uriUserInfo = ""
          , uriRegName  = auth
          , uriPort     = ""
          }
      , uriPath     = buildPath path
      , uriQuery    = buildQuery query
      , uriFragment = ""
      }


{-------------------}
{- Query Fragments -}
{-------------------}

sanitizeQuery :: String -> String
sanitizeQuery = escapeURIString encode
  where
    encode c = not $ (isReserved c) && (c /= '=')

buildQuery :: [String] -> String
buildQuery ps =
  let
    qs = intercalate "&"
           $ filter (/= "")
           $ map sanitizeQuery
           $ ps
  in
    if null qs
      then ""
      else '?' : qs


queryOneOf :: (Show t) => Maybe t -> String
queryOneOf Nothing  = ""
queryOneOf (Just x) = show x

queryValid :: (Show t) => Maybe t -> String -> String
queryValid Nothing  _   = ""
queryValid (Just x) key = key ++ "=" ++ show x

queryYesNo :: Maybe YesNo -> String -> String -> String
queryYesNo x yes no = case x of
  Nothing  -> ""
  Just Yes -> yes
  Just No  -> no


{------------------}
{- Path Fragments -}
{------------------}

sanitizePath :: String -> String
sanitizePath = escapeURIString encode
  where
    encode c = not $ isReserved c

buildPath :: [String] -> String
buildPath = concatMap ('/':) . filter (/= "") . map sanitizePath

pathValid :: (Show t) => Maybe t -> String
pathValid Nothing  = ""
pathValid (Just x) = show x

pathValidPre :: (Show t) => String -> Maybe t -> [String]
pathValidPre _ Nothing  = []
pathValidPre p (Just x) = [p, show x]

pathYesNo :: Maybe YesNo -> String -> String -> String
pathYesNo x yes no = case x of
  Nothing  -> ""
  Just Yes -> yes
  Just No  -> no

pathYesNoPre :: String -> Maybe YesNo -> String -> String -> [String]
pathYesNoPre p x yes no = case x of
  Nothing  -> []
  Just Yes -> [p,yes]
  Just No  -> [p,no]


{--------------}
{- Attributes -}
{--------------}

attrValid :: (Monoid a, Show b) => (H.AttributeValue -> a) -> Maybe b -> a
attrValid key Nothing  = mempty
attrValid key (Just x) = key $ H.stringValue $ show x

