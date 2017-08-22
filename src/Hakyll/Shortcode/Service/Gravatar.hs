{-|
Copyright   : (c) Nathan Bloomfield, 2017
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hakyll.Shortcode.Service.Gravatar(
  expandGravatarShortcodes
) where

import Hakyll.Shortcode.Service
import Hakyll.Shortcode.Render
import Hakyll.Shortcode.Types

import Data.Monoid
import Network.URI
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Hash.MD5 (md5s)


data GravatarEmbed = GravatarEmbed
  { gv_hash          :: Maybe MD5_Digest
  , gv_class         :: Maybe Css_Class_Name
  , gv_size          :: Maybe Natural_Number_Base_10
  , gv_default       :: Maybe DefaultGravatar
  , gv_force_default :: Maybe YesNo
  , gv_rating        :: Maybe Rating
  } deriving Show


{- default gravatar style -}

data DefaultGravatar
 = FourOhFour
 | MysteryMan
 | Identicon
 | MonsterID
 | Wavatar
 | Retro
 | Blank
 deriving (Eq, Show)

instance QueryParameter DefaultGravatar where
  renderQueryParameter x = case x of
    FourOhFour -> "d=404"
    MysteryMan -> "d=mm"
    Identicon  -> "d=identicon"
    MonsterID  -> "d=monsterid"
    Wavatar    -> "d=wavatar"
    Retro      -> "d=retro"
    Blank      -> "d=blank"


{- rating -}

data Rating
 = GRating
 | PGRating
 | RRating
 | XRating
 deriving (Eq, Show)

instance QueryParameter Rating where
  renderQueryParameter x = case x of
    GRating  -> "r=g"
    PGRating -> "r=pg"
    RRating  -> "r=r"
    XRating  -> "r=x"


-- | Find and replace @gravatar@ shortcodes.
expandGravatarShortcodes :: String -> String
expandGravatarShortcodes =
  expandShortcodes (emptycode :: GravatarEmbed)


-- | Constructs the embed URI of a GeoGebraEmbed.
embedUri :: GravatarEmbed -> H.AttributeValue
embedUri GravatarEmbed{..} = H.stringValue
  $ buildURL HTTPS "www.gravatar.com" path query []
  where
    path =
      [ "avatar"
      , pathValid gv_hash
      ]

    query =
      [ queryValid gv_size "s"
      , queryOneOf gv_default
      , queryYesNo gv_force_default "f=y" ""
      , queryOneOf gv_rating
      ]



instance Shortcode GravatarEmbed where
  tag = ShortcodeTag "gravatar"


  emptycode = GravatarEmbed
    -- String Properties
    { gv_hash          = Nothing
    , gv_class         = validateMaybe "gravatar-container"
    , gv_size          = Nothing
    , gv_force_default = Nothing
    , gv_default       = Nothing
    , gv_rating        = Nothing
    }


  embedcode gv@GravatarEmbed{..}
    | gv_hash /= Nothing = do
        renderHtml $ do
          H.div H.! (attrValid A.class_ gv_class) $ do
            H.img H.! mconcat
              [ A.src $ embedUri gv
              ]

    | otherwise = missingError "gravatar" "hash"


  attributes =
    -- String Properties
    [ Valid "hash"  $ \x gv -> gv { gv_hash  = Just x }
    , Valid "class" $ \x gv -> gv { gv_class = Just x }
    , Valid "size"  $ \x gv -> gv { gv_size  = Just x }

    -- Yes/No Properties
    , YesNo "force-default" $ \x gv -> gv { gv_force_default = Just x }

    -- Enumerated Properties
    , OneOf "default"
        [ ("404",       \gv -> gv { gv_default = Just FourOhFour })
        , ("mystery",   \gv -> gv { gv_default = Just MysteryMan })
        , ("identicon", \gv -> gv { gv_default = Just Identicon  })
        , ("monsterid", \gv -> gv { gv_default = Just MonsterID  })
        , ("wavatar",   \gv -> gv { gv_default = Just Wavatar    })
        , ("retro",     \gv -> gv { gv_default = Just Retro      })
        , ("blank",     \gv -> gv { gv_default = Just Blank      })
        ]

    , OneOf "rating"
        [ ("g",  \gv -> gv { gv_rating = Just GRating  })
        , ("pg", \gv -> gv { gv_rating = Just PGRating })
        , ("r",  \gv -> gv { gv_rating = Just RRating  })
        , ("x",  \gv -> gv { gv_rating = Just XRating  })
        ]
    ]
