module Four
    (
    part1
    ) where

import Data.Hash.MD5
import Data.List
import Data.Maybe

part1 :: String -> String -> Int

part1 str toMatch=
  let longList = [0..10000000]
      found = find (\n -> take (length toMatch) (computeHash (str ++ show n)) == toMatch) longList
  in fromMaybe 0 found

computeHash str = md5s $ Str str
