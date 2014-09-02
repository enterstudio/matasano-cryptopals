{-# LANGUAGE OverloadedStrings #-}

-- http://cryptopals.com/sets/1/challenges/8/

import Data.Ord
import Data.List
import Control.Arrow
import Data.List.Split

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import qualified Data.Set as S

main :: IO ()
main = do
  cs <- BSLC8.lines `fmap` BSL.readFile "../files/8.txt"
  mapM_ display $ take 5 $ sortBy (comparing snd) $ map (id *** rank) $ zip [0..] cs

rank :: BSLC8.ByteString -> Int
rank c = S.size $ S.fromList $ chunksOf 16 $ BSL.unpack c

display :: (Int, Int) -> IO ()
display (a,b) = putStrLn $ "Line: " ++ show a ++ ", Distinct 16-Byte Blocks: " ++ show b
