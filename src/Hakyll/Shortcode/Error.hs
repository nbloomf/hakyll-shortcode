module Hakyll.Shortcode.Error where

typeError :: String -> String -> String -> String -> String
typeError tag key badval expect = concat
  [ "(Nb. there is an error in this '" ++ tag ++ "' shortcode; "
  , "the value '" ++ badval ++ "' for key '" ++ key ++ "' was not expected. "
  , expect ++ ")"
  ]

missingError :: String -> String -> String
missingError tag key = concat
  [ "(Nb. there is an error in this '" ++ tag ++ "' shortcode; "
  , "you must provide a value for the '" ++ key ++ "' key.)"
  ]

validateError :: String -> String -> String -> String -> String
validateError tag key badval expect = concat
  [ "(Nb. there is an error in this '" ++ tag ++ "' shortcode; "
  , "the value '" ++ badval ++ "' for key '" ++ key ++ "' is invalid. "
  , expect ++ ")"
  ]
