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

-- Extract the votes from each row in the dataset as a two tuple of (candidate index, vote priority)
-- e.g. [(0,5),(1,1),(2,3),(3,4),(4,2)]
buildVotes :: [String] -> [(Int, String)]
buildVotes xs = [(i, x) | (i, x) <- zip [0..] $ drop 2 xs]

-- Takes the second element of two tuples to build a list of them
secondElementToList :: [(a, b)] -> [b]
secondElementToList = map (snd)

-- Removes all empty or asterik votes from rows
cleanInvalidVotes :: [(Int, String)] -> [(Int, String)]
cleanInvalidVotes = filter (\x -> snd x /= "" && snd x /= "*")

-- Converts all two tuples into fully integers
changeVotesToInts :: [(Int, String)] -> [(Int, Int)]
changeVotesToInts xs = [(x, read y :: Int) | (x, y) <- xs]

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

-- Stop counting votes for the row if any duplicates are detected
cleanDuplicateVotes :: [(Int, Int)] -> [(Int, Int)]
cleanDuplicateVotes [] = []
cleanDuplicateVotes (x : xs)
  | snd x `elem` secondElementToList xs = []
  | otherwise                           = x : cleanDuplicateVotes xs

-- Stops counting votes for the row if any gap more than one between vote priorities are detected
cleanGappedVotes :: [(Int, Int)] -> [(Int, Int)]
cleanGappedVotes [] = []
cleanGappedVotes (x : xs)
  | snd x + 1 `notElem` secondElementToList xs  = [x]
  | otherwise                                   = x : cleanGappedVotes xs

-- Bundle all the invalid vote parsing as one operation
validateVotes :: [(Int, String)] -> [(Int, Int)]
validateVotes xs = cleanGappedVotes $ cleanDuplicateVotes $ sortVotes $ changeVotesToInts $ cleanInvalidVotes xs

-- Takes the first element of two tuples to build a list of them
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
cleanVotes ukVotes = map (\x -> getCandidatesFromIndexes (firstElementToList $ validateVotes $ buildVotes x) candidates) filteredVotes
  where
    filteredVotes = removeBlankVotees $ tail $ splitToLists ukVotes
    candidates = getCandidates ukVotes