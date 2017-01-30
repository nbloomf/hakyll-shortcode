module Tests.Hakyll.Shortcode.Service.GeoGebra (
  service_test_geogebra
) where

import Hakyll.Shortcode

import Test.Tasty
import Test.Tasty.HUnit


service_test_geogebra :: TestTree
service_test_geogebra = testGroup "geogebra"
  [ geogebra_missing_id
  ]

geogebra_missing_id :: TestTree
geogebra_missing_id = testCase "geogebra: no 'id' key" $ do
  let text = "<p>[geogebra]</p>"
  let proc = expandShortcodes [GeoGebra] text
  proc @?= "(Nb. there is an error in this 'geogebra' shortcode; you must provide a value for the 'id' key.)"

