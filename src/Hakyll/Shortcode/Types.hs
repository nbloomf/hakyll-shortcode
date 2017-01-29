{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental

The types reexported by this module (except for 'YesNo', which is a renaming of 'Bool') are safe strings. Each is an instance of the 'Validate' class, and may only be constructed using the 'validate' function. This ensures that the elements of a safe string type satisfy the constraints given in their 'Validate' instance.
-}

module Hakyll.Shortcode.Types (
  module Hakyll.Shortcode.Validate,

  module Hakyll.Shortcode.Types.Css_Class_Name,
  module Hakyll.Shortcode.Types.Domain_With_Scheme,
  module Hakyll.Shortcode.Types.Hex_Color_Code,
  module Hakyll.Shortcode.Types.Iso_639_1_Language_Code,
  module Hakyll.Shortcode.Types.Letters_Numbers,
  module Hakyll.Shortcode.Types.Letters_Numbers_Hyphens_Underscores,
  module Hakyll.Shortcode.Types.Natural_Number_Base_10,
  module Hakyll.Shortcode.Types.RFC_3986_Unreserved_Uri_Characters,
  module Hakyll.Shortcode.Types.YesNo,

  module Hakyll.Shortcode.Types.CommaSep
) where


import Hakyll.Shortcode.Validate

import Hakyll.Shortcode.Types.Css_Class_Name
import Hakyll.Shortcode.Types.Domain_With_Scheme
import Hakyll.Shortcode.Types.Hex_Color_Code
import Hakyll.Shortcode.Types.Iso_639_1_Language_Code
import Hakyll.Shortcode.Types.Letters_Numbers
import Hakyll.Shortcode.Types.Letters_Numbers_Hyphens_Underscores
import Hakyll.Shortcode.Types.Natural_Number_Base_10
import Hakyll.Shortcode.Types.RFC_3986_Unreserved_Uri_Characters
import Hakyll.Shortcode.Types.YesNo

import Hakyll.Shortcode.Types.CommaSep
