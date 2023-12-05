{-# LANGUAGE OverloadedStrings #-}

module Day4(day4) where

import Data.Text (splitOn, unpack, pack, Text)
import Data.Char (isDigit)
import Data.List.Split (wordsBy)
import InputData (getInputData)
import Data.List (intersect, nub)

data Card = Card{idNo :: Int, winVals :: [Int], vals :: [Int]} deriving (Show, Eq)

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

getCard :: Int -> Card -> Bool
getCard i card = (idNo card) == i

newCards :: [Card] -> Card -> [Card]
newCards cards c = do
  let wins = length (winningNumbers c)
  concatMap (\cId -> filter (getCard cId) cards) [((idNo c)+1)..((idNo c)+wins)]

newCards1 :: [Card] -> [Card] -> [Card]
newCards1 orgcards [] = []
newCards1 orgcards (card:rest) = card : newCards1 orgcards ((newCards orgcards card) ++ rest)

day4 :: IO()
day4 = do
  respb <- getInputData 4
  let cards = (map parseCards (lines respb))
  putStrLn "--- PART 1 -------------------------"
  print (sum (map calcPoints (map winningNumbers cards)))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  let nnCards = newCards1 cards cards
  print (length nnCards)
  putStrLn "------------------------------------"
