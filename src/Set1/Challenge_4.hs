{-

Set 1 - Challenge 4 (https://cryptopals.com/sets/1/challenges/4)
===================

Detect single-character XOR

One of the 60-character strings in this file has been encrypted by single-character XOR.

Find it.  (Your code from #3 should help.)

Found it: "Now that the party is jumping"

-}

module Set1.Challenge_4 where

import Data.Bits     (xor)
import Data.Ord      (comparing)
import Data.List     (sortBy)
import Data.Maybe
import Control.Applicative
import qualified Set1.Challenge_3 as C3

main = do
  ls <- lines <$> readFile "files/4.txt"
  mapM_ putStrLn $ take 1
                 $ sortBy (comparing C3.rank)
                 $ concat
                 $ map fn ls

fn l = mapMaybe (fmap C3.asString . xorEncoded l) [0..254]

xorEncoded l n = fmap (map (xor n)) (C3.bytes `fmap` C3.hexDecode l)
