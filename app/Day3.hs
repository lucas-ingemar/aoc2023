{-# LANGUAGE OverloadedStrings #-}

module Day3(day3) where

import Data.Char (isAlphaNum, isDigit)
import Data.List (elemIndex, nubBy, foldl')
import Data.Maybe (mapMaybe)
import Text.Regex.Posix

import InputData (getInputData)

data Coord = Coord {row :: Int, col :: Int} deriving (Show, Eq)
data PartNumber = PartNumber{value :: Int, pRow :: Int, pColStart :: Int, pColEnd :: Int} deriving (Show, Eq)

checkBoolean :: [Bool] -> Bool
checkBoolean xs =
    if all (== False) xs then False
    else True

eachLine :: (Int, String) -> [(Bool, Coord)]
eachLine (lineIdx, s) = map (eachChar lineIdx) (zipWith (\idx c -> (idx, c)) [0..] s)

eachChar :: Int -> (Int, Char) -> (Bool, Coord)
eachChar lineIdx (charIdx, c) = (not (isAlphaNum c), Coord{ row=lineIdx, col=charIdx})

eachLineP2 :: (Int, String) -> [(Bool, Coord)]
eachLineP2 (lineIdx, s) = map (eachCharP2 lineIdx) (zipWith (\idx c -> (idx, c)) [0..] s)

eachCharP2 :: Int -> (Int, Char) -> (Bool, Coord)
eachCharP2 lineIdx (charIdx, c) = (c == '*', Coord{ row=lineIdx, col=charIdx})

fixCoords :: (bool, Coord) -> Coord
fixCoords (_, coord) = coord

findNumbers :: String -> String
findNumbers l = takeWhile isDigit $ dropWhile (not . isDigit) l

getValuesPos :: String -> [(Int, Int)]
getValuesPos l = getAllMatches ( l =~ ("[0-9]+" :: String)) :: [(Int, Int)]

getValues :: String -> [String]
getValues l = (getAllTextMatches ( l =~ ("[0-9]+" :: String)) :: [String])

generatePartNumber :: Int -> ((Int, Int), String) -> PartNumber
generatePartNumber lidx ((startPos, endPos), value) = PartNumber{value=(read value), pRow=lidx, pColStart=startPos, pColEnd=(startPos + endPos - 1)}

getPartNumbers :: (Int, String) -> [PartNumber]
getPartNumbers (lIdx, l) = map (generatePartNumber lIdx) (zip (getValuesPos l) (getValues l))

isCoordinate :: [Coord] -> Int -> Int -> Bool
isCoordinate coords row col = (Coord row col) `elem` coords

isActualPartNumber :: [Coord] -> PartNumber -> Bool
isActualPartNumber coords part = do
  checkBoolean $ map (\col -> isCoordinate coords ((pRow part) - 1) col) [((pColStart part) - 1)..((pColEnd part) + 1)]
       ++ map (\col -> isCoordinate coords (pRow part) col) [((pColStart part) - 1), ((pColEnd part) + 1)]
       ++ map (\col -> isCoordinate coords ((pRow part) + 1) col) [((pColStart part) - 1)..((pColEnd part) + 1)]


isPartInRange :: (Int, Int) -> PartNumber -> Bool
isPartInRange (row, col) part = pRow part == row && pColStart part <= col && pColEnd part >= col

listSurroundingParts :: [PartNumber] -> Coord -> [PartNumber]
listSurroundingParts parts coord = nubBy eqByValue (concat (map (\col -> filter (isPartInRange (row coord - 1, col)) parts) [col coord - 1..col coord + 1]
  ++ map (\col -> filter (isPartInRange (row coord, col)) parts) [col coord - 1, col coord + 1]
  ++ map (\col -> filter (isPartInRange (row coord + 1, col)) parts) [col coord - 1..col coord + 1]))

eqByValue :: PartNumber -> PartNumber -> Bool
eqByValue part1 part2 = value part1 == value part2 && pColStart part1 == pColStart part2 && pRow part1 == pRow part2

multiplyParts :: [PartNumber] -> Int
multiplyParts p = foldl' (*) 1 (map value p)

calcCoord :: [PartNumber] -> Coord -> Int
calcCoord parts coord = multiplyParts (listSurroundingParts parts coord)

day3 :: IO()
day3 = do
  let replacePeriods = map (\c -> if c=='.' then 'a'; else c)
  respb <- getInputData 3

  let values =(concat (map getPartNumbers (zipWith (\idx c -> (idx, c)) [0..] (lines (respb)))))

  putStrLn "--- PART 1 -------------------------"
  let coordList = (concat (map eachLine (zipWith (\idx c -> (idx, c)) [0..] (lines (replacePeriods respb)))))
  let coords = (map fixCoords (filter (\(boolVal, _) -> boolVal) coordList))
  print (sum (map value (filter (isActualPartNumber coords) values)))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  let coordList2 = (concat (map eachLineP2 (zipWith (\idx c -> (idx, c)) [0..] (lines (replacePeriods respb)))))
  let coords = (map fixCoords (filter (\(boolVal, _) -> boolVal) coordList))
  print coords
  print (multiplyParts (listSurroundingParts values (coords!!0)))
  print (sum (map (calcCoord values) coords))
  putStrLn "------------------------------------"
-- Jag tror att man bara ska kolla en rak linje. Nu kollar jag i V form osv ocksa
