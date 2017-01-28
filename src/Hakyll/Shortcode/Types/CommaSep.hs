module Hakyll.Shortcode.Types.CommaSep (
  CommaSep()
) where


import Data.List (intercalate)
import Data.List.Split (splitOn)

import Hakyll.Shortcode.Validate


data CommaSep t
  = Make { unMake :: [t] } deriving Eq

instance (Validate t) => Validate (CommaSep t) where
  validate = fmap Make . sequence . map validate . splitOn ","

instance (Validate t, Show t) => Show (CommaSep t) where
  show = intercalate "," . map show . unMake
