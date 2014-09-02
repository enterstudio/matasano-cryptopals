
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BSL
import Data.Array as A
import Data.Bits
import Data.Digits

hexify :: Integral a => a -> String
hexify nums = pad str
  where
  digs = map fromIntegral $ digits 16 nums
  str  = map (table !) digs

table :: Array Int Char
table = A.listArray (0,15) $ ['0'..'9'] ++ ['a'..'z']

pad :: String -> String
pad [ ] = "00"
pad [x] = ['0',x]
pad  xs = xs

answerB64String :: String
answerB64String = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
               ++ "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

text :: BSL.ByteString
text = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

keyCycle :: BSL.ByteString
keyCycle = BSL.cycle "ICE"

answerBS :: String
answerBS = concat $ BSL.zipWith ((hexify .) . xor) text keyCycle

main :: IO ()
main = print $ answerB64String == answerBS
