{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

module Hakyll.Shortcode.Types.CommaSep (
  CommaSep()
) where


import Data.List (intercalate)
import Data.List.Split (splitOn)

import Hakyll.Shortcode.Validate


-- | Comma-separated lists of valid strings.
data CommaSep t
  = Make { unMake :: [t] } deriving Eq

instance (Validate t) => Validate (CommaSep t) where
  validate text = case sequence . map validate . splitOn "," $ text of
    Left er -> Left $ er ++ " (Separated by commas.)"
    Right x -> Right (Make x)

instance (Validate t, Show t) => Show (CommaSep t) where
  show = intercalate "," . map show . unMake
