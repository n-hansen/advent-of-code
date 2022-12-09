module P7Spec where

import TestPrelude

import Puzzles.P7 (Command(..),DirectoryListing(..),FileSystem(..),buildFs)
import qualified Puzzles.P7 as P
import Parse

pt1 = puzzleExample P.inputParser1 P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser2 P.pt2 "pt 2"

spec_p7 :: Spec
spec_p7 = do
  describe "example 1" $ do
    let input = [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|]

    specify "parser" $
      parseEither P.inputParser1 input
      `shouldBe`
      Right [ Cd "/"
            , Ls [ Dir "a"
                 , File 14848514 "b.txt"
                 , File 8504156 "c.dat"
                 , Dir "d"
                 ]
            , Cd "a"
            , Ls [ Dir "e"
                 , File 29116 "f"
                 , File 2557 "g"
                 , File 62596 "h.lst"
                 ]
            , Cd "e"
            , Ls [ File 584 "i" ]
            , Cd ".."
            , Cd ".."
            , Cd "d"
            , Ls [ File 4060174 "j"
                 , File 8033020 "d.log"
                 , File 5626152 "d.ext"
                 , File 7214296 "k"
                 ]
            ]

    specify "buildFs" $
      buildFs <$> parseEither P.inputParser1 input
      `shouldBe`
      Right (FS [ ("b.txt", 14848514)
                , ("c.dat", 8504156)
                ]
              [ ("a", FS [ ("f", 29116)
                         , ("g", 2557)
                         , ("h.lst", 62596)
                         ]
                      [ ("e", FS [("i",584)] [])]
                )
              , ("d", FS [ ("j", 4060174)
                         , ("d.log", 8033020)
                         , ("d.ext", 5626152)
                         , ("k", 7214296)
                         ] [])
              ]
            )

    pt1 input 95437
    pt2 input 24933642
