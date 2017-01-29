{-|
Copyright  : (c) Nathan Bloomfield, 2017
License    : GPL-3
Maintainer : nbloomf@gmail.com
Stability  : experimental

Helper functions for constructing URLs and HTML fragments.
-}

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


-- | Simple sum type representing URL schemes.
data Scheme
  = HTTPS

instance Show Scheme where
  show HTTPS = "https"


-- | Helper function for safely building URLs.
buildURL
  :: Scheme   -- ^ The scheme
  -> String   -- ^ The domain (not including ://)
  -> [String] -- ^ List of path components, to be separated by /.
  -> [String] -- ^ List of query components, to be separated by &.
  -> [String] -- ^ List of fragment components.
  -> String
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


-- | Helper function for rendering @Maybe@ 'OneOf' shortcode parameters as query parameters.
queryOneOf :: (Show t)
  => Maybe t -- ^ The 'OneOf' parameter.
  -> String
queryOneOf Nothing  = ""
queryOneOf (Just x) = show x

-- | Helper function for rendering @Maybe@ 'Valid' shortcode parameters as query parameters.
queryValid :: (Show t)
  => Maybe t -- ^ The 'Valid' parameter.
  -> String  -- ^ The parameter key.
  -> String
queryValid Nothing  _   = ""
queryValid (Just x) key = key ++ "=" ++ show x

-- | Helper function for rendering @Maybe@ 'YesNo' shortcode parameters as query parameters.
queryYesNo
  :: Maybe YesNo -- ^ The 'YesNo' parameter.
  -> String      -- ^ Parameter for the 'Yes' case.
  -> String      -- ^ Parameter for the 'No' case.
  -> String
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

-- | Helper function for rendering @Maybe@ 'Valid' parameters as path components.
pathValid :: (Show t) => Maybe t -> String
pathValid Nothing  = ""
pathValid (Just x) = show x

-- | Helper function for rendering @Maybe@ 'Valid' parameters as path components, with a prefix.
pathValidPre :: (Show t)
  => String  -- ^ The prefix path.
  -> Maybe t -- ^ The 'Valid' parameter.
  -> [String]
pathValidPre _ Nothing  = []
pathValidPre p (Just x) = [p, show x]

-- | Helper function for rendering @Maybe@ 'YesNo' parameters as path components.
pathYesNo
  :: Maybe YesNo -- ^ The 'YesNo' parameter.
  -> String      -- ^ Path for the 'Yes' case.
  -> String      -- ^ Path for the 'No' case.
  -> String
pathYesNo x yes no = case x of
  Nothing  -> ""
  Just Yes -> yes
  Just No  -> no

-- | Helper function for rendering @Maybe@ 'YesNo' parameters as path components, with a prefix.
pathYesNoPre
  :: String      -- ^ The prefix path.
  -> Maybe YesNo -- ^ The 'YesNo' parameter.
  -> String      -- ^ Path for the 'Yes' case.
  -> String      -- ^ Path for the 'No' case.
  -> [String]
pathYesNoPre p x yes no = case x of
  Nothing  -> []
  Just Yes -> [p,yes]
  Just No  -> [p,no]


{--------------}
{- Attributes -}
{--------------}

-- | Helper function for optionally rendering a @Maybe@ as an HTML attribute.
attrValid :: (Monoid a, Show b) => (H.AttributeValue -> a) -> Maybe b -> a
attrValid key Nothing  = mempty
attrValid key (Just x) = key $ H.stringValue $ show x

