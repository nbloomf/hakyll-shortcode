module Hakyll.Shortcode.Types.Iso_639_1_Language_Code (
  Iso_639_1_Language_Code()
) where

import Hakyll.Shortcode.Validate

newtype Iso_639_1_Language_Code
  = Make { unMake :: String } deriving Eq

instance Validate Iso_639_1_Language_Code where
  validate text = if elem text codes
    then Right $ Make text
    else Left "Must be an ISO639.1 two-letter language code."
    where
      codes =
        [ "ab", "aa", "af", "ak", "sq", "am", "ar", "an", "hy", "as"
        , "av", "ae", "ay", "az", "bm", "ba", "eu", "be", "bn", "bh"
        , "bi", "bs", "br", "bg", "my", "ca", "ch", "ce", "ny", "zh"
        , "cv", "kw", "co", "cr", "hr", "cs", "da", "dv", "nl", "dz"
        , "en", "eo", "et", "ee", "fo", "fj", "fi", "fr", "ff", "gl"
        , "ka", "de", "el", "gn", "gu", "ht", "ha", "he", "hz", "hi"
        , "ho", "hu", "ia", "id", "ie", "ga", "ig", "ik", "io", "is"
        , "it", "iu", "ja", "jv", "kl", "kn", "kr", "ks", "kk", "km"
        , "ki", "rw", "ky", "kv", "kg", "ko", "ku", "kj", "la", "lb"
        , "lg", "li", "ln", "lo", "lt", "lu", "lv", "gv", "mk", "mg"
        , "ms", "ml", "mt", "mi", "mr", "mh", "mn", "na", "nv", "nd"
        , "ne", "ng", "nb", "nn", "no", "ii", "nr", "oc", "oj", "cu"
        , "om", "or", "os", "pa", "pi", "fa", "pl", "ps", "pt", "qu"
        , "rm", "rn", "ro", "ru", "sa", "sc", "sd", "se", "sm", "sg"
        , "sr", "gd", "sn", "si", "sk", "sl", "so", "st", "es", "su"
        , "sw", "ss", "sv", "ta", "te", "tg", "th", "ti", "bo", "tk"
        , "tl", "tn", "to", "tr", "ts", "tt", "tw", "ty", "ug", "uk"
        , "ur", "uz", "ve", "vi", "vo", "wa", "cy", "wo", "fy", "xh"
        , "yi", "yo", "za", "zu"
        ]

instance Show Iso_639_1_Language_Code where
  show = unMake
