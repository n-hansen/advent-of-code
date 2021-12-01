module UtilSpec where

import TestPrelude

import Util

spec_util = describe "utils" $ do
  specify "window" $
      window 3 [1..5]
      `shouldBe`
      [ [1,2,3]
      , [2,3,4]
      , [3,4,5]
      ]

  specify "window2" $
    window2 [1..4]
    `shouldBe`
    [ (1,2)
    , (2,3)
    , (3,4)
    ]

  specify "window3" $
    window3 [1..5]
    `shouldBe`
    [ (1,2,3)
    , (2,3,4)
    , (3,4,5)
    ]
