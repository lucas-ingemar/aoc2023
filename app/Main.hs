{-# LANGUAGE OverloadedStrings #-}

import Day1(day1)
import Day2(day2)
import Day3(day3)
import Day4(day4)
import Day5(day5)


main :: IO()
main = do
  putStrLn "DAY 1"
  day1
  putStrLn "\nDAY 2"
  day2
  putStrLn "\nDAY 3"
  day3
  -- putStrLn "\nDAY 4"
  -- day4
  putStrLn "\nDAY 5"
  day5
