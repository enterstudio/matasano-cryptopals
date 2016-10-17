{-# LANGUAGE OverloadedStrings #-}

{-

Set 1 - Challenge 7 (http://cryptopals.com/sets/1/challenges/7/)
===================

AES in ECB mode

The Base64-encoded content in this file has been encrypted via AES-128 in ECB
mode under the key

"YELLOW SUBMARINE".

(case-sensitive, without the quotes; exactly 16 characters; I like "YELLOW
SUBMARINE" because it's exactly 16 bytes long, and now you do too).

Decrypt it. You know the key, after all.

Easiest way: use OpenSSL::Cipher and give it AES-128-ECB as the cipher.

Do this with code.

You can obviously decrypt this using the OpenSSL command-line tool, but we're
having you get ECB working in code for a reason. You'll need it a lot later on,
and not just for attacking ECB.

-}

module Set1.Challenge_7 where

import Crypto.Cipher.AES
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Base64.Lazy as BSB64

main :: IO ()
main = do
  ls <- BSL.readFile "files/7.txt"
  run $ (BSB64.decode . BSL.concat . BSLC8.lines) ls

pass :: BS.ByteString
pass = "YELLOW SUBMARINE"

run :: Show a => Either a BSLC8.ByteString -> IO ()
run (Left err) = print err
run (Right xs) = do
  let aes = initAES pass
      dec = decryptECB aes (BSL.toStrict xs)

  BSC8.putStrLn $ BS.take 200 dec
