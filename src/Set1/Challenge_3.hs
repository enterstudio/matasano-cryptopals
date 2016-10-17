{-

Set 1 - Challenge 3 (http://cryptopals.com/sets/1/challenges/3/)

The hex encoded string:

1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736
... has been XOR'd against a single character. Find the key, decrypt the message.

You can do this by hand. But don't: write code to do it for you.

How? Devise some method for "scoring" a piece of English plaintext.
Character frequency is a good metric. Evaluate each output and choose the
one with the best score.

-}

module Set1.Challenge_3 where

import Safe          (headMay)
import Numeric       (readHex)
import Data.Ord      (comparing)
import Data.Char     (chr)
import Data.Bits     (xor)
import Data.List     (sortBy)
import Data.Maybe    (mapMaybe)

main = mapM_ putStrLn
     $ take 1
     $ sortBy   (comparing rank)
     $ mapMaybe (fmap asString . xorEncoded) [0..254]

rank = negate . length . filter (`elem` ['a'..'z'])

asString = map (chr . fromInteger)

xorEncoded :: Integer -> Maybe [Integer]
xorEncoded n = fmap (map (xor n)) encodedBytes

hexDecode :: String -> Maybe Integer
hexDecode = fmap fst . headMay . readHex

encodedBytes = bytes `fmap` hexDecode encoded

encoded = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

numAsString = map (chr . fromInteger) . bytes

bytes = reverse . bytesRev

bytesRev n | (d,m) == (0,0) = []
           | otherwise      = m : bytesRev d
  where (d,m) = divMod n 256
