module Count.SingleTransferableVote where

import Debug.Trace

------------------------------------------------------
-- Algorithm 2: Single Transferable Vote
------------------------------------------------------

-- Master quota calculator function used in Driver
getQuota :: [[String]] -> Int -> Float -> Float
getQuota cleanedVotes seatCount weight = (changeIntToFloat (length cleanedVotes) / (changeIntToFloat (seatCount) + 1) + 1) * weight

changeIntToFloat :: Int -> Float
changeIntToFloat x = fromIntegral x :: Float

-- Takes the first element of two tuples to build a list of them
firstElementToList :: [(a, b)] -> [a]
firstElementToList = map (fst)

-- Master vote results calculation function used in Driver
-- Output result: [(Elected, Total Votes Weighting, Surplus, Round, New Weighting Factor)]
-- singleTransferableVote :: [[String]] -> [(Int, String)] -> Int -> Float -> [(String, Float, Float, Int, Float)]
-- singleTransferableVote cleanedVotes candidates seatCount weight =
--   getSTVResultsSummary cleanedVotes candidates seatCount weight weight quota [] 1
--     where
--       quota = getQuota cleanedVotes seatCount weight

-- findVotesForCandidate :: [String] -> String -> Float -> ([String], Float)
-- findVotesForCandidate allVotes candidate currentWeight =
--   map (\x -> (x, currentWeight)) $ filter (\x -> x !! 0 == candidate) allVotes

-- aggregateRanking :: [(String, Float, [([String], Float)])] -> [(String, ([String], Float))] -> Float -> [(String, Float, [([String], Float)])]
-- aggregateRanking [] candidateVotes currentWeight = [ (candidate, (length newVotes) * currentWeight, newVotes ) | (candidate, newVotes) <- candidateVotes ] 
-- aggregateRanking currentRanking candidateVotes currentWeight =
--   [ (candidate, voteWeighting + ((length newVotes) * currentWeight), aggregatedVotes ++ newVotes) | (candidate, voteWeighting, aggregatedVotes) <- currentRanking, (_candidate, newVotes) <- candidateVotes, candidate == _candidate ]

-- addVotesToRanking :: [(String, Float, [([String], Float)])] -> [[String]] -> [(Int, String)] -> Float -> [(String, Float, [([String], Float)])]
-- addVotesToRanking currentRanking allVotes candidates currentWeight
--   | length allVotes > 0 = aggregateRanking currentRanking candidateVotes currentWeight
--   | otherwise           = currentRanking
--     where
--       candidateVotes = map (\x -> (snd x, findVotesForCandidate allVotes (snd x) currentWeight)) candidates

-- getSTVResultsSummary :: [[String]] -> [(Int, String)] -> Int -> Float -> Float -> Float -> [(String, Float, [([String], Float)])] -> [(String, Float, Float, Int, Float)]
-- getSTVResultsSummary allVotes candidates 0 originalWeight currentWeight quota currentRanking round = []
-- getSTVResultsSummary allVotes candidates seatCount originalWeight currentWeight quota currentRanking round =
--   getSTVResultsSummary updatedVotes updatedCandidates updatedSeats originalWeight updatedWeight quota newRanking (round + 1)
--     where
--       updatedVotes = addVotesToRanking currentRanking updatedVotes candidates currentWeight
--

getCurrentRoundRanking :: [[String]] -> [(Int, String)] -> [(String, [[String]])]
getCurrentRoundRanking cleanedVotes candidates = map (\x -> (snd x, filter (\y -> y !! 0 == snd x) cleanedVotes)) candidates

addRankings :: [(String, [[String]])] -> [(String, [[String]])] -> [(String, [[String]])]
addRankings oldRanking [] = oldRanking
addRankings [] newRanking = newRanking
addRankings oldRanking newRanking = [(x, y ++ z) | (x, y) <- newRanking, (a, z) <- oldRanking, x == a]

-- Sorting rankings by descending order
sortRanking :: Ord b => [(a, [[b]])] -> [(a, [[b]])]
sortRanking [] = []
sortRanking (x : xs) = insertionSortDescending x $ sortRanking xs

insertionSortDescending :: Ord b => (a, [[b]]) -> [(a, [[b]])] -> [(a, [[b]])]
insertionSortDescending x [] = [x]
insertionSortDescending x (y : ys) 
  | length (snd x) >= length (snd y)  = x : y : ys
  | otherwise                         = y : insertionSortDescending x ys

removeCandidateFromListing :: [(Int, String)] -> (String, [[String]]) -> [(Int, String)]
removeCandidateFromListing candidates candidate = filter (\x -> snd x /= fst candidate) candidates

redistributeVotesFromCandidate :: [(String, [[String]])] -> (String, [[String]]) -> [[String]]
redistributeVotesFromCandidate updatedRanking removedFromContest =
  map (tail) $ filter (\xs -> (length xs > 1) && xs !! 1 `elem` candidatesLeft) $ snd removedFromContest
    where
      candidatesLeft = firstElementToList updatedRanking

adjustWeightingToSurplus :: Float -> Float -> Float -> (String, [[String]]) -> [[String]] -> Float
adjustWeightingToSurplus originalWeight previousWeight quota removedFromContest updatedVotes =
  if transferableVoteCount <= surplus then previousWeight else originalWeight * (surplus / nonTransferableVoteCount)
    where
      nonTransferableVoteCount = originalWeight * changeIntToFloat (length $ updatedVotes)
      transferableVoteCount = originalWeight * changeIntToFloat (length $ snd removedFromContest)
      surplus = transferableVoteCount - quota

-- Master vote results calculation function used in Driver
singleTransferableVote :: [[String]] -> [(Int, String)] -> Int -> Float -> [(String, Int, Float)]
singleTransferableVote cleanedVotes candidates seatCount weight =
  electCandidatesSTV cleanedVotes candidates seatCount weight weight quota []
    where
      quota = getQuota cleanedVotes seatCount weight

electCandidatesSTV :: [[String]] -> [(Int, String)] -> Int -> Float -> Float -> Float -> [(String, [[String]])] -> [(String, Int, Float)]
electCandidatesSTV cleanedVotes candidates 0 originalWeight weight quota currentRanking = []
electCandidatesSTV cleanedVotes candidates seatCount originalWeight weight quota currentRanking =
  (if pastQuota then [(fst highestRanking, length (snd highestRanking), updatedWeight)] else []) ++ electCandidatesSTV updatedVotes updatedCandidates updatedSeatCount originalWeight updatedWeight quota updatedRanking
    where
      updatedRanking = sortRanking $ addRankings (getCurrentRoundRanking cleanedVotes candidates) currentRanking
      highestRanking = updatedRanking !! 0
      lowestRanking = updatedRanking !! (length (updatedRanking) - 1)
      pastQuota = (changeIntToFloat $ length (snd highestRanking)) * weight >= quota || length updatedRanking <= seatCount
      removedFromContest = if pastQuota then highestRanking else lowestRanking
      updatedCandidates = removeCandidateFromListing candidates removedFromContest
      updatedVotes = redistributeVotesFromCandidate updatedRanking removedFromContest
      updatedWeight = if pastQuota then adjustWeightingToSurplus originalWeight weight quota removedFromContest updatedVotes else originalWeight
      updatedSeatCount = if pastQuota then seatCount - 1 else seatCount