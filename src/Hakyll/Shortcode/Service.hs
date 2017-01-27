{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Hakyll.Shortcode.Service (
  Shortcode,
  tag, attributes, emptycode, embedcode,
  ShortcodeTag(ShortcodeTag),
  ShortcodeAttribute(YesNo, OneOf, Valid),
  expandShortcodes,
  missingError,
  YesNo(..)
) where

import Hakyll.Shortcode.Validate
import Hakyll.Shortcode.Parser
import Hakyll.Shortcode.YesNo

import Control.Monad (foldM)
import Data.List (intercalate)
import Data.List.Utils (replace)
import Text.ParserCombinators.Parsec
import Text.Regex.Posix



{-----------------------}
{- The Shortcode Class -}
{-----------------------}

class Shortcode t where
  -- The tag for our shortcode
  tag :: ShortcodeTag t

  -- The allowed keys for our shortcode
  attributes :: [ShortcodeAttribute t]

  -- An empty shortcode instance
  emptycode :: t

  -- Convert t to HTML
  embedcode :: t -> String


data ShortcodeTag a = ShortcodeTag
  { unTag :: String
  } deriving Show


data ShortcodeAttribute t where
  -- The key string and a mutator.
  YesNo :: String -> (YesNo -> t -> t) -> ShortcodeAttribute t

  -- The key string and a list of value/mutator pairs.
  OneOf :: String -> [(String, t -> t)] -> ShortcodeAttribute t

  -- The key string and a mutator taking a Validate type.
  Valid :: (Validate a) => String -> (a -> t -> t) -> ShortcodeAttribute t




-- Update t with a keyval pair
update :: (Shortcode t) => t -> (String, String) -> Either String t
update x kv = foldM (processKeyVal kv) x attributes
  where
    processKeyVal :: forall t. (Shortcode t)
      => (String, String) -> t -> ShortcodeAttribute t -> Either String t

    processKeyVal (k,v) x (YesNo key f)
      | key /= k  = Right x
      | otherwise = case v of
          "yes" -> Right $ f Yes x
          "no"  -> Right $ f No  x
          _     -> Left $ typeError (unTag theTag) key v ["\"yes\"", "\"no\""]
          where
            theTag :: ShortcodeTag t
            theTag = tag

    processKeyVal (k,v) x (OneOf key cases)
      | key /= k  = Right x
      | otherwise = foo cases
          where
            foo [] = Left $ typeError (unTag theTag) key v $ map (show . fst) cases
            foo ((val,f):cs) = if val /= v
              then foo cs
              else Right $ f x
            
            theTag :: ShortcodeTag t
            theTag = tag

    processKeyVal (k,v) x (Valid key f)
      | key /= k  = Right x
      | otherwise = case validate v of
          Right z -> return $ f z x
          Left er -> Left $ validateError (unTag theTag) k v er
            where
              theTag :: ShortcodeTag t
              theTag = tag



{------------------------}
{- Expanding Shortcodes -}
{------------------------}

expandShortcodes :: (Shortcode t) => t -> String -> String
expandShortcodes x text = foldr (expandOne x) text matches
  where
    matches = getAllTextMatches $ text =~ (shortcodeRegex x)

    expandOne :: (Shortcode t) => t -> String -> String -> String
    expandOne x code text = replace code (getReplacement x code) text

    shortcodeRegex :: forall t. (Shortcode t) => t -> String
    shortcodeRegex x = "<p>\\[[[:blank:]]*" ++ (unTag theTag) ++ "[^]]*]</p>"
      where
        theTag :: ShortcodeTag t
        theTag = tag

    getReplacement :: forall t. (Shortcode t) => t -> String -> String
    getReplacement x text = case runParser p () "" text of
      Left err           -> parseError (unTag theTag) $ show err
      Right atts -> case foldM update init atts of
        Left err     -> err
        Right result -> embedcode result
      where
        p :: Parser [(String, String)]
        p = shortcodeParser (unTag theTag)

        init :: t
        init = emptycode

        theTag :: ShortcodeTag t
        theTag = tag


{----------}
{- Errors -}
{----------}

validateError :: String -> String -> String -> String -> String
validateError tag key badval expect = concat
  [ "(Nb. there is an error in this '" ++ tag ++ "' shortcode; "
  , "the value '" ++ badval ++ "' for key '" ++ key ++ "' is invalid. "
  , expect ++ ")"
  ]

typeError :: String -> String -> String -> [String] -> String
typeError tag key badval expects = concat
  [ "(Nb. there is an error in this '" ++ tag ++ "' shortcode; "
  , "the value '" ++ badval ++ "' for key '" ++ key ++ "' was not expected. "
  , "Possible values: " ++ (intercalate " " expects) ++ ".)"
  ]

parseError :: String -> String -> String
parseError tag err = concat
  [ "(Nb. there was an error while parsing this '" ++ tag ++ "' tag. "
  , err ++ ".)"
  ]

missingError :: String -> String -> String
missingError tag key = concat
  [ "(Nb. there is an error in this '" ++ tag ++ "' shortcode; "
  , "you must provide a value for the '" ++ key ++ "' key.)"
  ]
