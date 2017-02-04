{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental

Do you want to make more shortcodes? Of course, we all do.

This module demonstrates how to implement a type-safe shortcode.
The basic steps are as follows:

  1. Define a type @t@ which represents the state of a shortcode.
     Typically @t@ will be a record type, and the fields should all
     be Maybes or Lists of instances of the 'Validate' class.

  2. Define a default instance of your shortcode state. This may
     consist of a bunch of @Nothing@s and @[]@s, or maybe you prefer
     nontrivial defaults. This is the 'emptycode' method of the
     'Shortcode' class.

  3. Declare the usable keys for your shortcode by giving a list of
     'ShortcodeAttribute t's. This type lets us define our keys in
     a declarative way, sort of like GetOpt. This is the 'attributes'
     method of the 'Shortcode' class.

  4. Provide a map @t -> String@, to be used to convert abstract
     shortcodes to their expanded form. This is the 'embedcode'
     method of the 'Shortcode' class.

  5. Last but not least, declare which tag will be used to name
     your shortcode.

And that's it! The shortcode API and type library will take care of
parsing, validation, and sanitization for you, giving you a function
@String -> String@ that expands your new shortcode while making sure
only input conforming to your type model is allowed.

Now let's walk through a sample shortcode module. To read along,
view the source of this module.
-}


-- | A couple of language extensions will be useful.
-- The OverloadedStrings extension is used by Text.Blaze
-- to minimize boilerplate. Similarly, RecordWildCards will
-- make working with large records less verbose.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- | We only need to export one function: an alias for
-- the 'expandShortcodes' function. More on this later.
module Hakyll.Shortcode.Service.Example(
  expandExampleShortcodes
) where


-- | Now for some imports; these bring in the Shortcode API.
import Hakyll.Shortcode.Service -- Handles parsing and expanding shortcodes.
import Hakyll.Shortcode.Render  -- Helper functions for rendering URLs and HTML.
import Hakyll.Shortcode.Types   -- A library of validatable string-like types.


-- | These modules are not necessary for defining a shortcode.
-- However, we will use them to more easily construct sanitized HTML.
import Data.Monoid
import Network.URI
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)


-- For the sake of an example, I want my shortcode to look like this:
--
--   [example valid='foo' item='bar' yesno='baz' oneof='qux']
--
-- subject to the following constraints:
--   1. foo should be a sequence of digits,
--   2. bar should be a sequence of letters and numbers;
--   3. baz should be either 'yes' or 'no'; and
--   4. qux should be either 'john', 'paul', 'george', or 'ringo'.
--
-- Moreover,
--   1. I want to require that 'valid' be present;
--   2. I only want one 'yesno' and 'oneof'; and
--   3. I want to be able to give zero or more 'list' values.


-- | We can represent this information using the following type.
-- Note that @Maybe@ means "zero or one" and @[]@ means "zero or more".
data Example = Example
  { ex_valid :: Maybe Natural_Number_Base_10
  , ex_list  :: [Letters_Numbers]
  , ex_yesno :: Maybe YesNo
  , ex_oneof :: Maybe Beatle
  }

-- | The 'Decimal_Digits', 'Letters_Numbers', and 'YesNo' types
-- are provided by 'Hakyll.Shortcode.Types'. But the 'Beatle' type
-- is custom, so we need to define it.
data Beatle
  = John
  | Paul
  | George
  | Ringo
  deriving Show


-- | This is the function that finds and replaces @example@
-- shortcodes. It is just a type-specific version of the
-- 'expandShortcodes' function, that takes a shortcode specification
-- and generates a find-and-replace function for it.
expandExampleShortcodes :: String -> String
expandExampleShortcodes =
  expandShortcodes (emptycode :: Example)



instance Shortcode Example where
  tag = ShortcodeTag "example"


  emptycode = Example
    -- String Properties
    { ex_valid = Nothing
    , ex_list  = []
    , ex_yesno = Nothing
    , ex_oneof = Nothing
    }


  -- | The 'embedcode' function renders a shortcode object as text;
  -- typically HTML, but here we'll just use plain text for simplicity.
  embedcode ex@Example{..} 
    | ex_valid /= Nothing = unlines
        [ "Here are the arguments of your example shortcode:"
        , "  " ++ show ex_valid
        , "  " ++ show ex_list
        , "  " ++ show ex_yesno
        , "  " ++ show ex_oneof
        ]

    | otherwise = "Missing 'valid' key!"


  attributes =
    [ Valid "valid" $ \x ex -> ex { ex_valid = Just x }
    , Valid "item"  $ \x ex -> ex { ex_list  = x : ex_list ex }
    , YesNo "yesno" $ \x ex -> ex { ex_yesno = Just x }
    , OneOf "oneof"
        [ ("john",   \ex -> ex { ex_oneof = Just John   })
        , ("paul",   \ex -> ex { ex_oneof = Just Paul   })
        , ("george", \ex -> ex { ex_oneof = Just George })
        , ("ringo",  \ex -> ex { ex_oneof = Just Ringo  })
        ]
    ]

