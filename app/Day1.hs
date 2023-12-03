{-# LANGUAGE OverloadedStrings #-}

module Day1(day1) where

import Data.Char (isDigit)
import Data.Text(pack, unpack, replace)
import InputData (getInputData)

filterDigits :: String -> String
filterDigits val = filter isDigit val

firstLastDigits :: String -> String
firstLastDigits val = (take 1 val) ++ [last val]

stringToInt :: String -> Int
stringToInt s = read s

replaceString :: String -> String
replaceString = unpack .
  replace "nine" "nine9nine" .
  replace "eight" "eight8eight" .
  replace "seven" "seven7seven" .
  replace "six" "six6six" .
  replace "five" "five5five" .
  replace "four" "four4four" .
  replace "three" "three3three" .
  replace "two" "two2two" .
  replace "one" "one1one" .
  pack

day1 :: IO()
day1 = do
  respb <- getInputData 1
  putStrLn "--- PART 1 -------------------------"
  print (sum (map stringToInt (map firstLastDigits (map filterDigits (lines respb)))))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  print (sum (map stringToInt (map firstLastDigits (map filterDigits (map replaceString (lines respb))))))
  putStrLn "------------------------------------"
