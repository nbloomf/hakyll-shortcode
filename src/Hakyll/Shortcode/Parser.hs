{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Parser (
  shortcodeParser
) where

import Text.ParserCombinators.Parsec


{----------------------}
{- Parsing Shortcodes -}
{----------------------}

-- | Given a tag, parses strings of the form @<p>[tag key1='val1' ... keyN='valN']</p>@ into lists of @(keyI,valI)@ pairs.
shortcodeParser :: String -> Parser [(String, String)]
shortcodeParser tag = do
  string "<p>["
  many $ char ' '
  string tag
  many $ char ' '
  attributes <- sepBy (try keyvalParser) (many $ char ' ')
  many $ char ' '
  string "]</p>"
  return attributes


keyvalParser :: Parser (String, String)
keyvalParser = do
  key <- many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['-']
  char '='
  value <- quotedString
  return (key, value)


quotedString :: Parser String
quotedString = foldl1 (<|>)
  [ delimitedString '\''     '\''
  , delimitedString '"'      '"'
  , delimitedString '\x2018' '\x2019' -- handed single quotes
  , delimitedString '\x201C' '\x201D' -- handed double quotes
  ]
  where
    delimitedString :: Char -> Char -> Parser String
    delimitedString open close = do
      _ <- char open
      t <- many $ choice
             [ try $ noneOf [ close, '\\', '\n', ']' ]
             , try $ string ['\\',close] >> return close
             , try $ string "\\\\"       >> return '\\'
             , try $ string "\\n"        >> return '\n'
             , try $ string "\\]"        >> return ']'
             ]
      _ <- char close
      return t
