{-# LANGUAGE OverloadedStrings #-}

module Day4(day4) where

import Data.Text (splitOn, unpack, pack, Text)
import Data.Char (isDigit)
import Data.List.Split (wordsBy)
import InputData (getInputData)
import Data.List (intersect, nub)

removeCardTag :: String -> [Text]
removeCardTag s = splitOn "|" (last (splitOn ":" (pack s)))

convertToInt :: [Text] -> [[Int]]
convertToInt s = map (\t -> map read (wordsBy (not . isDigit) (unpack t))) s

winningNumbers :: [[Int]] -> [Int]
winningNumbers l = nub (intersect (head l) (last l))

calcPoints :: [Int] -> Int
calcPoints l = if length l == 0 then 0 else 2^((length l) - 1)

day4 :: IO()
day4 = do
  respb <- getInputData 4
  putStrLn "--- PART 1 -------------------------"
  print (sum (map calcPoints (map winningNumbers (map convertToInt (map removeCardTag (lines respb))))))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  putStrLn "------------------------------------"
