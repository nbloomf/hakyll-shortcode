{-# LANGUAGE ScopedTypeVariables #-}

module Hakyll.Shortcode.Parser (
  Shortcode, tag, update, emptycode, embedcode,
  ShortcodeTag(ShortcodeTag),
  expandShortcodes
) where

import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Data.List.Utils (replace)
import Control.Monad (foldM)



{-----------------------}
{- The Shortcode Class -}
{-----------------------}

class Shortcode t where
  -- The tag for our shortcode
  tag :: ShortcodeTag t

  -- Update t with a keyval pair
  update :: t -> (String, String) -> Either String t

  -- An empty shortcode instance
  emptycode :: t

  -- Convert t to HTML
  embedcode :: t -> String


data ShortcodeTag a = ShortcodeTag
  { unTag :: String
  } deriving Show



{----------------------}
{- Parsing Shortcodes -}
{----------------------}

shortcode :: (Shortcode t) => Parser (Either String t)
shortcode = shortcodeParser tag emptycode


shortcodeParser :: (Shortcode a) => (ShortcodeTag a) -> a -> Parser (Either String a)
shortcodeParser (ShortcodeTag tag) init = do
  string "<p>["
  many $ char ' '
  string tag
  many $ char ' '
  attributes <- sepBy (try keyvalParser) (many $ char ' ')
  many $ char ' '
  string "]</p>"
  return $ foldM update init attributes


keyvalParser :: Parser (String, String)
keyvalParser = do
  key <- many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['-']
  char '='
  value <- quotedString
  return (key, value)


quotedString :: Parser String
quotedString = singleQuotedString <|> doubleQuotedString
  where
    singleQuotedString :: Parser String
    singleQuotedString = do
      _ <- char '\''
      t <- many $ choice
             [ try $ noneOf ['\\', '\'', '\n']
             , try $ string "\\n"  >> return '\n'
             , try $ string "\\'"  >> return '\''
             , try $ string "\\"   >> return '\\'
             ]
      _ <- char '\''
      return t  

    doubleQuotedString :: Parser String
    doubleQuotedString = do
      _ <- char '"'
      t <- many $ choice
             [ try $ noneOf ['\\', '"', '\n']
             , try $ string "\\n"  >> return '\n'
             , try $ string "\\\"" >> return '"'
             , try $ string "\\"   >> return '\\'
             ]
      _ <- char '"'
      return t



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
      Left error           -> "(shortcode error: " ++ show error
      Right (Left err)     -> err
      Right (Right result) -> embedcode result
      where
        p :: Parser (Either String t)
        p = shortcode
