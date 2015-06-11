module FileOperations where

import Data.Path
import Data.Array
import Data.Maybe
import Data.Foldable

allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> [Path]
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> [Path]
onlyFiles = filter isFile <<< allFiles'
  where
  isFile = not <<< isDirectory

largest :: Path -> Maybe Path
largest path = foldl largest' Nothing (onlyFiles path)
  where
  largest' (Just a) b = if (size a) > (size b) then Just a else Just b
  largest' Nothing b = Just b
