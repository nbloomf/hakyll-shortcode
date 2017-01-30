module Tests.Main where

import Hakyll.Shortcode

import Tests.Hakyll.Shortcode.Service.YouTube
import Tests.Hakyll.Shortcode.Service.GeoGebra

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Shortcode Tests"
  [ service_test_youtube
  , service_test_geogebra
  ]
