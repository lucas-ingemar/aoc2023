{-# LANGUAGE OverloadedStrings #-}

module Day2(day2) where

import Data.Char (isDigit)
import InputData (getInputData)
import Data.Text (splitOn, pack, unpack, Text, replace, isInfixOf)

splitGame :: String -> [Text]
splitGame s = splitOn ":" (pack s)

splitGameDropId :: String -> Text
splitGameDropId s = last (splitOn ":" (pack s))

splitGives :: [Text] -> Int
splitGives s = if checkBoolean (map splitCubes (splitOn ";" (last s))) then read (filterDigits (unpack (head s))) else 0

splitCubes :: Text -> Bool
splitCubes s = checkBoolean (map isPossibleOutcome (splitOn "," s))

filterDigits :: String -> String
filterDigits val = filter isDigit val

isPossibleOutcome :: Text -> Bool
isPossibleOutcome t =
  case () of
    _ | isInfixOf "red" t -> isPossibleNumber t 12
      | isInfixOf "green" t -> isPossibleNumber t 13
      | isInfixOf "blue" t -> isPossibleNumber t 14
      | otherwise -> False

isPossibleNumber :: Text -> Int -> Bool
isPossibleNumber s maxVal = read (filterDigits (unpack s)) <= maxVal

checkBoolean :: [Bool] -> Bool
checkBoolean xs =
    if all (== True) xs then True
    else False

getCubes :: Text -> String -> Int
getCubes l color = maximum (map (\item -> countColor item color) (splitOn ";" l))

countColor :: Text -> String -> Int
countColor t color = maximum (map (\item -> getColorValue item color) (splitOn "," t))

getColorValue :: Text -> String -> Int
getColorValue t color = if isInfixOf (pack color) t then read (filterDigits (unpack t)) else 0

day2 :: IO()
day2 = do
  respb <- getInputData 2

  putStrLn "--- PART 1 -------------------------"
  print (sum (map splitGives (map splitGame (lines respb))))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  let r = map (\item -> getCubes item "red")  (map splitGameDropId (lines respb))
  let g = map (\item -> getCubes item "green")  (map splitGameDropId (lines respb))
  let b = map (\item -> getCubes item "blue")  (map splitGameDropId (lines respb))
  print (sum (zipWith3 (\x y z -> x*y*z) r g b))
  putStrLn "------------------------------------"
