module P18Spec where

import TestPrelude

import Parse

import Puzzles.P18 hiding (pt1,pt2)
import qualified Puzzles.P18 as P

pt1 = puzzleExample inputParser P.pt1 "pt 1"
pt2 = puzzleExample inputParser P.pt2 "pt 2"

snumber = fromMaybe (panic "invalid number") . parseMaybe snParser

spec_p18 :: Spec
spec_p18 = do
  describe "zipper" $ do
    specify "traversal test" $
      unfoldr (fmap (node &&& identity) . next)
      (zipper $ snumber "[[1,2],[3,4]]")
      `shouldBe`
      [ snumber "[1,2]"
      , RegularNumber 1
      , RegularNumber 2
      , snumber "[3,4]"
      , RegularNumber 3
      , RegularNumber 4
      ]

    specify "left edit 1" $
      ( snumber "[1,2]"
        & zipper
        & next >>= next
        & fmap (editLNum (* 10))
        & fmap root
      )
      `shouldBe`
      Just (snumber "[10,2]")

    specify "left edit 2" $
      ( snumber "[1,2]"
        & zipper
        & next
        & fmap (editLNum (* 10))
        & fmap root
      )
      `shouldBe`
      Just (snumber "[1,2]")

    specify "left edit 3" $
      ( snumber "[[1,[2,3]],[4,5]]"
        & zipper
        & next >>= next >>= next >>= next >>= next >>= next >>= next
        & fmap (editLNum (* 10))
        & fmap root
      )
      `shouldBe`
      Just (snumber "[[1,[2,30]],[4,5]]")

    specify "right edit 1" $
      ( snumber "[1,2]"
        & zipper
        & next
        & fmap (editRNum (* 10))
        & fmap root
      )
      `shouldBe`
      Just (snumber "[1,20]")

    specify "right edit 2" $
      ( snumber "[1,2]"
        & zipper
        & next >>= next
        & fmap (editRNum (* 10))
        & fmap root
      )
      `shouldBe`
      Just (snumber "[1,2]")

    specify "right edit 3" $
      ( snumber "[[0,1],[[2,3],4]]"
        & zipper
        & next >>= next >>= next
        & fmap (editRNum (* 10))
        & fmap root
      )
      `shouldBe`
      Just (snumber "[[0,1],[[20,3],4]]")

    specify "edit test" $
      ( snumber "[[1,[2,3]],[[4,5],6]]"
        & zipper
        & next >>= next >>= next >>= next >>= next -- looking at 3
        & fmap (editLNum (* 10))
        & fmap (editRNum (const 200))
        >>= next >>= next >>= next >>= next -- looking at 5
        & fmap (edit (const $ snumber "[-1,-2]"))
        >>= next >>= next
        >>= editNum (* 1000)
        & fmap root
      )
      `shouldBe`
       (Just . snumber $ "[[1,[20,3]],[[200,[-1,-2000]],6]]")

  describe "explode" $ do
    let shouldExplodeTo input expect =
          (explode $ snumber input)
          `shouldBe`
          Just (snumber expect)

    specify "example 1" $
      "[[[[[9,8],1],2],3],4]"
      `shouldExplodeTo`
      "[[[[0,9],2],3],4]"

    specify "example 2" $
      "[7,[6,[5,[4,[3,2]]]]]"
      `shouldExplodeTo`
      "[7,[6,[5,[7,0]]]]"

    specify "example 3" $
      "[[6,[5,[4,[3,2]]]],1]"
      `shouldExplodeTo`
      "[[6,[5,[7,0]]],3]"

    specify "example 4" $
      "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
      `shouldExplodeTo`
      "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

  describe "split" $ do
    let shouldSplitTo input expect =
          (split $ snumber input)
          `shouldBe`
          Just (snumber expect)

    specify "example 1" $
      "[1,10]"
      `shouldSplitTo`
      "[1,[5,5]]"

    specify "example 2" $
      "[11,10]"
      `shouldSplitTo`
      "[[5,6],10]"

  describe "addition" $ do
    specify "example" $
      add (snumber "[[[[4,3],4],4],[7,[[8,4],9]]]") (snumber "[1,1]")
      `shouldBe`
      snumber "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

  describe "example 1" $ do
    let input = [r|[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
|]

    pt1 input 4140
    pt2 input 3993
