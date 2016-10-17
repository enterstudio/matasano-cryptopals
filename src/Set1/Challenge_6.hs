{-# LANGUAGE OverloadedStrings #-}

{-

Set 1 - Challenge 6 (http://cryptopals.com/sets/1/challenges/6/)
===================

Break repeating-key XOR

It is officially on, now.

This challenge isn't conceptually hard, but it involves actual error-prone
coding. The other challenges in this set are there to bring you up to speed.
This one is there to qualify you. If you can do this one, you're probably just
fine up to Set 6.

There's a file here. It's been base64'd after being encrypted with repeating-key XOR.

Decrypt it.

Here's how:

Let KEYSIZE be the guessed length of the key; try values from 2 to (say) 40.

Write a function to compute the edit distance/Hamming distance between two
strings. The Hamming distance is just the number of differing bits. The
distance between: "this is a test" and "wokka wokka!!!" is 37. Make sure your
code agrees before you proceed.

For each KEYSIZE, take the first KEYSIZE worth of bytes, and the second KEYSIZE
worth of bytes, and find the edit distance between them. Normalize this result
by dividing by KEYSIZE.

The KEYSIZE with the smallest normalized edit distance is probably the key. You
could proceed perhaps with the smallest 2-3 KEYSIZE values. Or take 4 KEYSIZE
blocks instead of 2 and average the distances.

Now that you probably know the KEYSIZE: break the ciphertext into blocks of
KEYSIZE length.

Now transpose the blocks: make a block that is the first byte of every block,
and a block that is the second byte of every block, and so on.

Solve each block as if it was single-character XOR. You already have code to do
this.

For each block, the single-byte XOR key that produces the best looking
histogram is the repeating-key XOR key byte for that block. Put them together
and you have the key.

This code is going to turn out to be surprisingly useful later on. Breaking
repeating-key XOR ("Vigenere") statistically is obviously an academic exercise,
a "Crypto 101" thing. But more people "know how" to break it than can actually
break it, and a similar technique breaks something much more important.

No, that's not a mistake.

We get more tech support questions for this challenge than any of the other
ones. We promise, there aren't any blatant errors in this text. In particular:
the "wokka wokka!!!" edit distance really is 37.

-}

module Set1.Challenge_6 where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC8
import Data.ByteString.Base64.Lazy
import Data.Bits
import Control.Lens
import Data.Bits.Lens
import Data.Ord
import Data.Maybe
import Data.List
import Data.List.Split
import GHC.Word
import Control.Arrow
import Control.Monad

diffbits :: BSL.ByteString -> BSL.ByteString -> Int
diffbits x y = length $ filter id $ BSL.zipWith xor x y & concatMap (toListOf bits)

diffbitsl :: [BSL.ByteString] -> Maybe Int
diffbitsl [x,y] = Just $ diffbits x y
diffbitsl _     = Nothing

prop_diffbits :: Bool
prop_diffbits = diffbits "this is a test" "wokka wokka!!!" == 37

kstest :: BSL.ByteString -> Int -> Double
kstest s n' = fromIntegral (sum $ mapMaybe diffbitsl items) / fromIntegral n'
  where
  n     = fromIntegral n'
  items = take 3 $ chunksOf 2 $ map (BSL.take n) $ iterate (BSL.drop (n*2)) s

setks :: [ Int ]
setks = [2..50] -- Possible key lengths

setkc :: [ Word8 ]
setkc = [minBound .. maxBound] -- Possible key characters

setan :: String
setan = "aeiou "

crackXor :: BSL.ByteString -> Word8
crackXor s = fst $ minimumBy (comparing snd) $ map (id &&& fitxor s) setkc

fitxor :: BSL.ByteString -> Word8 -> Double
fitxor s k = fitness $ BSL.map (xor k) s

fitness :: BSL.ByteString -> Double
fitness = fromIntegral . BSL.length . BSL.filter (flip BSL.notElem (BSC8.pack setan))

unscramble :: BSL.ByteString -> BSL.ByteString -> BSL.ByteString
unscramble k s = BSL.pack $ BSL.zipWith xor s (BSL.cycle k)

main :: IO ()
main = do
  ls <- BSL.readFile "files/6.txt"
  run $ (decode . BSL.concat . BSC8.lines) ls

run :: Either String BSL.ByteString -> IO ()
run (Left err) = print err
run (Right  c) = do
  putStrLn $ "Got Decoded Base64: " ++ show (BSL.take 13 c) ++ "..."

  let pairs  = zip setks $ map (kstest c) setks
      sorted = sortBy (comparing snd) pairs
      kss    = map fst $ take 10 sorted

  putStrLn $ "Got Key-Sizes:      " ++ show kss

  let keyps = keypsf c kss

  forM_ keyps $ \keyp -> putStrLn $ "Got Key:            " ++ show keyp

  forM_ keyps $ \keyp ->
    putStrLn $ "Got plain-text:     " ++ show (BSL.take 70 (unscramble keyp c)) ++ "..."

keypsf ::  Monad m => BSC8.ByteString -> m Int -> m BSC8.ByteString
keypsf c kss = do
  ks <- kss
  let cs   = map BSL.pack $ transpose $ chunksOf ks $ BSL.unpack c
      key  = map crackXor cs
      keyp = BSL.pack key
  return keyp
