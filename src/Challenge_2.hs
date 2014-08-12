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

xor = (+)
hexDecode = const 1
hexEncode = const "hello"

prop_pass = "746865206b696420646f6e277420706c6179"
         == hexEncode ( hexDecode "1c0111001f010100061a024b53535009181c"
                  `xor` hexDecode "686974207468652062756c6c277320657965" )
