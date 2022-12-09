{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P7 where

import AocPrelude
import Parse
import Puzzle

import Control.Lens.TH
import Control.Monad.Writer.Strict
import qualified Data.Sequence as S

type Input1 = [Command]
type Input2 = Input1

data Command
  = Ls [DirectoryListing]
  | Cd Text
  deriving (Show,Eq)

data DirectoryListing
  = Dir Text
  | File Int Text
  deriving (Show,Eq)

data FileSystem
  = FS { _files :: Map Text Int
       , _dirs :: Map Text FileSystem
       } deriving (Show, Eq)

data FSZipper =
  FSZ { _zFocus :: FileSystem
      , _zParent :: Maybe (Text, FSZipper)
      } deriving Show

makeLenses ''FileSystem
makeLenses ''FSZipper

p7 :: Puzzle
p7 = Puzzle "7" inputParser1 inputParser2 pt1 pt2

fsZipper :: FileSystem -> FSZipper
fsZipper fs = FSZ fs Nothing

rezip :: FSZipper -> FileSystem
rezip z | atRoot z = z ^. zFocus
        | otherwise = rezip $ cdUp z

atRoot :: FSZipper -> Bool
atRoot z = isNothing $ z ^. zParent

cdDown :: Text -> FSZipper -> FSZipper
cdDown d z = FSZ focus $ Just (d,z)
  where
    focus = fromMaybe emptyFs $ z ^. zFocus . dirs . at d

cdUp :: FSZipper -> FSZipper
cdUp z =
  case z ^. zParent of
    Nothing -> panic "no parent directory"
    Just (d,p) -> p & zFocus . dirs . at d ?~ (z ^. zFocus)

makeFile :: Text -> Int -> FileSystem -> FileSystem
makeFile name size = files . at name ?~ size

makeDir :: Text -> FileSystem -> FileSystem
makeDir name = dirs . at name ?~ emptyFs

emptyFs :: FileSystem
emptyFs = FS mempty mempty

inputParser1 :: Parser Input1
inputParser1 = many command
  where
    command = string "$ " >> cmdLs <|> cmdCd
    cmdLs = do
      string "ls\n"
      Ls <$> lsResults `endBy1` newline
    lsResults = dirListing <|> fileListing
    dirListing = string "dir " >> Dir <$> takeWhile1P (Just "directory name") (/= '\n')
    fileListing =
      File
      <$> unsignedInteger
      <* space1
      <*> takeWhile1P (Just "file name") (/= '\n')
    cmdCd = string "cd " >> Cd <$> takeWhile1P Nothing (/= '\n') <* newline

buildFs :: [Command] -> FileSystem
buildFs = rezip . foldl' (flip handleCmd) (fsZipper emptyFs)
  where
    handleCmd (Cd "/") = fsZipper . rezip
    handleCmd (Cd "..") = cdUp
    handleCmd (Cd d) = cdDown d
    handleCmd (Ls ls) = foldl' (flip handleLsEntry) `flip` ls
    handleLsEntry (Dir n) = zFocus %~ makeDir n
    handleLsEntry (File s n) = zFocus %~ makeFile n s

dirSizes :: FileSystem -> Seq Int
dirSizes = execWriter . go
  where
    go fs = do
      children <- traverse go (fs ^.. dirs . traverse)
      let myFiles = sumOf (files . traverse) fs
          mySize = myFiles + sum children
      tell $ S.singleton mySize
      pure mySize

inputParser2 :: Parser Input2
inputParser2 = inputParser1

pt1 = Just . sum . filter (<= cutoff) . toList . dirSizes . buildFs
  where
    cutoff = 100000

pt2 input = Just answer
  where
    totalFsSize = 70000000
    neededSpace = 30000000
    ds = toList . dirSizes . buildFs $ input
    freeSpace = totalFsSize - maximum ds
    needToFree = neededSpace - freeSpace
    answer = minimum . filter (>= needToFree) $ ds
