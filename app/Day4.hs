{-# LANGUAGE OverloadedStrings #-}

module Day4(day4) where

import Data.Text (splitOn, unpack, pack, Text)
import Data.Char (isDigit)
import Data.List.Split (wordsBy)
import InputData (getInputData)
import Data.List (intersect, nub)

data Card = Card{id :: Int, winVals :: [Int], vals :: [Int]} deriving (Show, Eq)

filterDigits :: String -> String
filterDigits val = filter isDigit val

convertToInt :: Text -> [Int]
convertToInt s = map read (wordsBy (not . isDigit) (unpack s))

parseCards :: String -> Card
parseCards s = do
  let dataList = splitOn ":" (pack s)
  let vals = splitOn "|" (last dataList)
  let tId = read (filterDigits (unpack (head dataList)))
  Card tId (convertToInt (head vals)) (convertToInt (last vals))

winningNumbers :: Card -> [Int]
winningNumbers c = nub (intersect (winVals c) (vals c))

calcPoints :: [Int] -> Int
calcPoints l = if length l == 0 then 0 else 2^((length l) - 1)

day4 :: IO()
day4 = do
  respb <- getInputData 4
  putStrLn "--- PART 1 -------------------------"
  print (sum (map calcPoints (map winningNumbers (map parseCards (lines respb)))))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  putStrLn "------------------------------------"
