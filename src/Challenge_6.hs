{-# LANGUAGE OverloadedStrings #-}

-- http://cryptopals.com/sets/1/challenges/6/

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
  ls <- BSL.readFile "../files/6.txt"
  run $ (decode . BSL.concat . BSC8.lines) ls

run :: Either String BSL.ByteString -> IO ()
run (Left err) = print err
run (Right  c) = do
  putStrLn $ "Got Decoded Base64: " ++ show (BSL.take 13 c) ++ "..."

  let pairs  = zip setks $ map (kstest c) setks
      sorted = sortBy (comparing snd) pairs
      kss    = map fst $ take 10 sorted

  putStrLn $ "Got Key-Sizes:      " ++ show kss

  let keyps = do
      ks <- kss
      let cs   = map BSL.pack $ transpose $ chunksOf ks $ BSL.unpack c
          key  = map crackXor cs
          keyp = BSL.pack key
      return keyp

  forM_ keyps $ \keyp -> putStrLn $ "Got Key:            " ++ show keyp

  forM_ keyps $ \keyp ->
    putStrLn $ "Got plain-text:     " ++ show (BSL.take 70 (unscramble keyp c)) ++ "..."
