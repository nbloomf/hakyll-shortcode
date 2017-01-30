module Tests.Hakyll.Shortcode.Service.YouTube (
  service_test_youtube
) where

import Hakyll.Shortcode

import Test.Tasty
import Test.Tasty.HUnit


service_test_youtube :: TestTree
service_test_youtube = testGroup "youtube"
  [ youtube_no_params
  ]

-- | Must provide either @id@ or both @list@ and @list-type@.
youtube_no_params :: TestTree
youtube_no_params = testCase "youtube: no parameters" $ do
  let text = "<p>[youtube]</p>"
  let proc = expandShortcodes [YouTube] text
  proc @?= "(Error: either the 'id' or the 'list' and 'list-type' parameter must be set.)"
