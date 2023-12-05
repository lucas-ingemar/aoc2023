{-# LANGUAGE OverloadedStrings #-}

module Day5(day5) where

import InputData (getInputData)
import Data.Text (splitOn, pack, unpack, Text)
import Text.Regex.Posix (AllTextMatches(getAllTextMatches), (=~))
import Data.List (elemIndex)


data ConvMap = ConvMap
  { catFrom :: String
  , catTo :: String
  , idList :: [(Int, Int, Int)]
  } deriving (Show, Eq)

getValues :: String -> [String]
getValues l = (getAllTextMatches ( l =~ ("[0-9]+" :: String)) :: [String])

getMap :: String -> ConvMap -> Bool
getMap cat cm = (catFrom cm) == cat

createIdVals :: [(Int, Int, Int)] -> [String] -> [(Int, Int, Int)]
createIdVals vals [] = vals
createIdVals vals (s:rest) =
  let v = map (read :: String -> Int) (getValues s)
      updatedVals = vals ++ [(v!!0, v!!1, v!!2)]
  in createIdVals updatedVals rest

filterMapVals :: Int -> (Int, Int, Int) -> Bool
filterMapVals wantedV (destS, sourceS, r) = sourceS <= wantedV && wantedV <= (sourceS + r)

getDestVal :: Int -> [(Int, Int, Int)] -> Int
getDestVal wantedV cmIdList = do
  let fd = filter (filterMapVals wantedV) cmIdList
  let idTuple = if ((length fd) > 0) then fd!!0 else (wantedV, wantedV, 0)
  let (destS, sourceS, r) = idTuple
  (destS + (wantedV - sourceS))

parseConvMaps :: Text -> ConvMap
parseConvMaps s = do
  let l = lines (unpack s)
  let fromTo = concat ((head l) =~ ("([a-z]+)-to-([a-z]+) map" :: String) :: [[String]])
  let vals = createIdVals [] (tail l)
  ConvMap (fromTo!!1) (fromTo!!2) vals

createSeedMap :: Int -> ConvMap
createSeedMap v = ConvMap "" "seed" [(v, 0, 0)]

convertToRangeVals :: [a] -> [(a, a)]
convertToRangeVals [] = []
convertToRangeVals [_] = []
convertToRangeVals (x1:x2:xs) = (x1, x2) : convertToRangeVals xs

createSeedMapPart2 ::  (Int, Int) -> [ConvMap]
createSeedMapPart2 (f, t) = map (\v -> ConvMap "" "seed" [(v, 0, 0)]) [f..((f+t)-1)]

calcPath :: [ConvMap] -> Int -> ConvMap -> (ConvMap, Int)
calcPath acm idx cm
  | catTo cm == "" = do
      calcPath acm idx ((filter (getMap "seed") acm)!!0)
  | catTo cm == "location" = do
      (cm, (getDestVal idx (idList cm)))
  | otherwise = do
      let tIdx = getDestVal idx (idList cm)
      calcPath acm tIdx ((filter (getMap (catTo cm)) acm)!!0)

calcPathPart2 :: [ConvMap] -> (Int, Int) -> Int
calcPathPart2 acm (sv, ev) = minimum (map (\i -> snd (calcPath acm i (ConvMap "" "" []))) [sv..(sv+ev)])

day5 :: IO()
day5 = do
  respb <- getInputData 5

  let rawMaps = splitOn "\n\n" (pack respb)
  let rawSeeds = map (read::String->Int) (getValues (unpack (head rawMaps)))
  let cmaps = map parseConvMaps (tail rawMaps)

  putStrLn "--- PART 1 -------------------------"
  let seeds = map createSeedMap rawSeeds
  print (minimum (map (\s -> snd (calcPath cmaps 0 s)) seeds))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  print (convertToRangeVals rawSeeds)
  let seeds = concat (map createSeedMapPart2 (convertToRangeVals rawSeeds))
  print (minimum (map (calcPathPart2 cmaps) (convertToRangeVals rawSeeds)))
  putStrLn "------------------------------------"
