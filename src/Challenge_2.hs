{- http://cryptopals.com/sets/1/challenges/2/
 -
 - Fixed XOR
 -
 - Write a function that takes two equal-length buffers and produces their XOR combination.
 -
 - If your function works properly, then when you feed it the string: 1c0111001f010100061a024b53535009181c
 - ... after hex decoding, and when XOR'd against:                    686974207468652062756c6c277320657965
 - ... should produce:                                                746865206b696420646f6e277420706c6179
 -}

module Challenge_2 where

import Safe
import Numeric
import Data.Bits

hexDecode :: String -> Maybe Integer
hexDecode = fmap fst . headMay . readHex
hexEncode = Just . ($ "") . showHex

prop_pass = Just "746865206b696420646f6e277420706c6179"
         == do a <- hexDecode "1c0111001f010100061a024b53535009181c"
               b <- hexDecode "686974207468652062756c6c277320657965"
               hexEncode (xor a b)
