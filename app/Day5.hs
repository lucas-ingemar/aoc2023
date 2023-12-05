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

calcPath :: [ConvMap] -> Int -> ConvMap -> (ConvMap, Int)
calcPath acm idx cm
  | catTo cm == "location" = do
      -- let tIdx  = maybeToInt idx (Map.lookup idx (idMap cm))
      (cm, (getDestVal idx (idList cm)))
  | otherwise = do
      -- let tIdx  = maybeToInt idx (Map.lookup idx (idMap cm))
      let tIdx = getDestVal idx (idList cm)
      calcPath acm tIdx ((filter (getMap (catTo cm)) acm)!!0)

day5 :: IO()
day5 = do
  respb <- getInputData 5
--   let respb = "seeds: 79 14 55 13\n\
-- \\n\
-- \seed-to-soil map:\n\
-- \50 98 2\n\
-- \52 50 48\n\
-- \\n\
-- \soil-to-fertilizer map:\n\
-- \0 15 37\n\
-- \37 52 2\n\
-- \39 0 15\n\
-- \\n\
-- \fertilizer-to-water map:\n\
-- \49 53 8\n\
-- \0 11 42\n\
-- \42 0 7\n\
-- \57 7 4\n\
-- \\n\
-- \water-to-light map:\n\
-- \88 18 7\n\
-- \18 25 70\n\
-- \\n\
-- \light-to-temperature map:\n\
-- \45 77 23\n\
-- \81 45 19\n\
-- \68 64 13\n\
-- \\n\
-- \temperature-to-humidity map:\n\
-- \0 69 1\n\
-- \1 0 69\n\
-- \\n\
-- \humidity-to-location map:\n\
-- \60 56 37\n\
-- \56 93 4"

  putStrLn "--- PART 1 -------------------------"
  let rawMaps = splitOn "\n\n" (pack respb)
  let seeds = map createSeedMap (map (read::String->Int) (getValues (unpack (head rawMaps))))
  let cmaps = map parseConvMaps (tail rawMaps)
  print (minimum (map (\s -> snd (calcPath cmaps 0 s)) seeds))
  putStrLn "------------------------------------"

  putStrLn "--- PART 2 -------------------------"
  putStrLn "------------------------------------"
