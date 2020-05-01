module Clean.CleanVotes where

import Data.List.Split (splitOn)

------------------------------------------------------
-- Data Cleaning Utilities
------------------------------------------------------

-- Split the raw votes to list of lists, creating a list by line and then by comma for each line
splitToLists :: String -> [[String]]
splitToLists ukVotes = map (splitOn ",") $ splitOn "\n" ukVotes

-- Count the number of empty strings from a list
countEmptyStrings :: [String] -> Int
countEmptyStrings xs = length $ filter (== "") xs

-- Remove rows that are entirely empty
removeBlankVotees :: [[String]] -> [[String]]
removeBlankVotees xss = filter (\xs -> countEmptyStrings xs /= length xs) xss

-- Master candidate listing function used in Driver
-- [(0,"D. Abbott"),(1,"E. Balls"),(2,"A. Burbhm"),(3,"D. Milliband"),(4,"E. Milliband")]
getCandidates :: String -> [(Int, String)]
getCandidates ukVotes = zip [0..] $ drop 2 $ head $ splitToLists ukVotes

-- Insertion sort algorithm for sorting two tuples' votes by ascending order for each votee
-- e.g. [(1,1),(4,2),(2,3),(3,4),(0,5)]
sortVotes :: Ord a => [(Int, a)] -> [(Int, a)]
sortVotes [] = []
sortVotes (x:xs) = insertionSort x $ sortVotes xs

insertionSort :: Ord a => (Int, a) -> [(Int, a)]  -> [(Int, a)]
insertionSort x [] = [x]
insertionSort x (y:ys) 
  | snd x <= snd y = x : y : ys
  | otherwise = y : insertionSort x ys

-- Extract the votes from each row in the dataset as a two tuple
-- e.g. [(0,5),(1,1),(2,3),(3,4),(4,2)]
buildVotes :: [String] -> [(Int, Int)]
buildVotes xs = [(az, read x :: Int) | (az, x) <- zip [0..] $ drop 2 xs, x /= "*", x /= ""]

-- Takes the first element of two tuples to build a list of them
-- e.g. [1,4,2,3,0]
firstElementToList :: [(Int, Int)] -> [Int]
firstElementToList = map (fst)

-- Maps indexes back to candidates based on the candidates listing
-- [(1,"E. Balls"),(4,"E. Milliband"),,(2,"A. Burbhm"),(3,"D. Milliband"),(0,"D. Abbott")]
getCandidatesFromIndexes :: [Int] -> [(Int, String)] -> [String]
getCandidatesFromIndexes indexes candidates = map (\x -> snd $ candidates !! x) indexes

-- Master vote cleaning function used in Driver
-- Builds a list of just Votes from each votee
-- e.g. [["E. Balls","E. Milliband","D. Milliband","A. Burbhm","D. Abbott"], ...]
cleanVotes :: String -> [[String]]
cleanVotes ukVotes = map (\x -> getCandidatesFromIndexes (firstElementToList (sortVotes (buildVotes x))) candidates) filteredVotes
  where
    filteredVotes = tail $ splitToLists ukVotes
    candidates = getCandidates ukVotes