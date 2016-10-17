{-

Set 1 - Challenge 1 (http://cryptopals.com/sets/1/challenges/1/)
===================

Convert hex to base64

The string:

49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d

Should produce:

SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t

So go ahead and make that happen. You'll need to use this code for the rest of the exercises.

Cryptopals Rule

Always operate on raw bytes, never on encoded strings. Only use hex and base64 for pretty-printing.

-}

module Set1.Challenge_1 where

import Data.Char (ord)
import Data.List.Split (chunksOf)

-- Solution

hexStringTo64String = concatMap (base64 . decodeHexToInt) . breakInto24BitChunks

-- Implementation

breakInto24BitChunks = chunksOf 6

decodeHexToInt s = sum $ zipWith (*) (reverse $ map numOrd s) (iterate (*16) 1)

numOrd a | elem a ['0'..'9'] = 00 + ord a - ord '0'
         | elem a ['A'..'F'] = 10 + ord a - ord 'A'
         | elem a ['a'..'f'] = 10 + ord a - ord 'a'
         | otherwise         = error "invalid 64 bit encoding"

base64 = reverse . base64Reversed

base64Reversed n | (d,m) == (0,0) = []
                 | otherwise      = toChar m : base64Reversed d
  where
  (d,m) = divMod n 64

toChar n = cycle set  !! n

set = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "/="

-- Acceptance Tests

prop_pass      = hexStringTo64String input == output
prop_set       = length set >= 64
prop_numOrd_a  = numOrd 'a' == 10
prop_numOrd_A  = numOrd 'A' == 10
prop_numOrd_0  = numOrd '0' == 0
prop_numOrd_9  = numOrd '9' == 9
prop_decode    = decodeHexToInt "ff" == 255
input          = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
output         = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

main :: IO ()
main = print $ hexStringTo64String input == output
