module Hakyll.Shortcode.Types.CommaSep (
  CommaSep()
) where


import Data.List (intercalate)
import Data.List.Split (splitOn)

import Hakyll.Shortcode.Validate


data CommaSep t
  = Make { unMake :: [t] } deriving Eq

instance (Validate t) => Validate (CommaSep t) where
  validate text = case sequence . map validate . splitOn "," $ text of
    Left er -> Left $ er ++ " (Separated by commas.)"
    Right x -> Right (Make x)

instance (Validate t, Show t) => Show (CommaSep t) where
  show = intercalate "," . map show . unMake
