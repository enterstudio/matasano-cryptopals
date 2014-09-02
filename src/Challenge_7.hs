{-# LANGUAGE OverloadedStrings #-}

-- http://cryptopals.com/sets/1/challenges/7/

import Crypto.Cipher.AES
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Base64.Lazy as BSB64

main :: IO ()
main = do
  ls <- BSL.readFile "../files/7.txt"
  run $ (BSB64.decode . BSL.concat . BSLC8.lines) ls

pass :: BS.ByteString
pass = "YELLOW SUBMARINE"

run :: Show a => Either a BSLC8.ByteString -> IO ()
run (Left err) = print err
run (Right xs) = do
  let aes = initAES pass
      dec = decryptECB aes (BSL.toStrict xs)

  BSC8.putStrLn $ BS.take 200 dec
