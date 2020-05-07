module Count.SingleTransferableVote where

import Clean.CleanVotes
import Debug.Trace

------------------------------------------------------
-- Algorithm 2: Single Transferable Vote
------------------------------------------------------

-- Master quota calculator function used in Driver
getQuota :: [[String]] -> Int -> Float -> Float
getQuota cleanedVotes seatCount weight = (changeIntToFloat (length cleanedVotes) / (changeIntToFloat (seatCount) + 1) + 1) * weight

-- Changes an integer value to a float
changeIntToFloat :: Int -> Float
changeIntToFloat x = fromIntegral x :: Float

-- Takes the first element of two tuples to build a list of them
firstElementToList :: [(a, b)] -> [a]
firstElementToList = map (fst)

-- Ensures that a vote belongs to the given candidate
ensureVoteForCandidate :: [String] -> String -> Int -> Bool
ensureVoteForCandidate vote candidate index = vote !! index == candidate

-- Breaks down the votes into a list of votes for each candidate, based on the given candidate listing
filterVotesForEachCandidate :: [[String]] -> [(Int, String)] -> [(String, [[String]])]
filterVotesForEachCandidate votes candidates =
  map (\x -> (snd x, filter (\y -> ensureVoteForCandidate y (snd x) 0) votes)) candidates

-- Updates the candidate's vote body if new votes exist
updateCandidatesVotes :: [(String, [(Float, [String])])] -> [(String, [[String]])] -> Float -> [(String, [(Float, [String])])]
updateCandidatesVotes [] candidatesVotes currentWeight =
  [(x, [ (currentWeight, a) | a <- y ]) | (x, y) <- candidatesVotes ]
updateCandidatesVotes updatedCandidatesVotes [] currentWeight = updatedCandidatesVotes
updateCandidatesVotes updatedCandidatesVotes candidatesVotes currentWeight =
  [ (x, y ++ b) | (x, y) <- updatedCandidatesVotes, (a, b) <- _candidatesVotes, x == a ]
    where
      _candidatesVotes = updateCandidatesVotes [] candidatesVotes currentWeight

-- Adds all of the floats together from a list
addAll :: [Float] -> Float
addAll (x : []) = x
addAll (x : xs) = x + addAll xs

-- Counts up all of the votes for each candidate to produce a ranking
countCandidatesVotes :: [(String, [(Float, [String])])] -> [(String, Float)]
countCandidatesVotes candidatesVotes = map (\x -> (fst x, addAll [y | (y, _) <- snd x ]) ) candidatesVotes

-- Sorts the ranking by descending order using the insertion sort algorithm
-- Similiar to the one implemented at Clean.CleanVotes
sortRanking :: Ord b => [(a, b)] -> [(a, b)]
sortRanking [] = []
sortRanking (x : xs) = insertionSortDescending x $ sortRanking xs

insertionSortDescending :: Ord b => (a, b) -> [(a, b)] -> [(a, b)]
insertionSortDescending x [] = [x]
insertionSortDescending x (y : ys) 
  | snd x >= snd y  = x : y : ys
  | otherwise       = y : insertionSortDescending x ys

-- Removes the given candidate from the listing
removeFromCandidatesList :: [(Int, String)] -> (String, Float) -> [(Int, String)]
removeFromCandidatesList candidates removedFromNextRound = filter (\x -> snd x /= fst removedFromNextRound) candidates

-- Removes the given candidate from the list of candidate votes
removeFromCandidateVotes :: [(String, [(Float, [String])])] -> (String, Float) -> [(String, [(Float, [String])])]
removeFromCandidateVotes candidatesVotes removedFromNextRound = filter (\x -> fst x /= fst removedFromNextRound) candidatesVotes

-- Filters out the invalid candidates from a vote
getNewVoteBodyForCandidate :: [String] -> [String] -> [String]
getNewVoteBodyForCandidate vote candidates = [ x | x <- vote, x `elem` candidates ]

-- Ensures that a vote belongs to a candidate in the given listing
ensureVoteForCandidates :: [String] -> [String] -> Bool
ensureVoteForCandidates vote candidates = (length (getNewVoteBodyForCandidate vote candidates)) > 0

-- Redistributes all of the votes belonging to a candidate to the master vote listing
redistributeVotes :: [(String, [(Float, [String])])] -> (String, Float) -> [(Int, String)] -> [[String]]
redistributeVotes _candidatesVotes removedFromNextRound _candidatesLeft =
  [ (getNewVoteBodyForCandidate b candidatesLeft) | (a,b) <- snd candidatesVotes, ensureVoteForCandidates b candidatesLeft ]
    where
      candidate = fst removedFromNextRound
      candidatesVotes = (filter (\x -> fst x == candidate) _candidatesVotes) !! 0
      candidatesLeft = secondElementToList _candidatesLeft

-- Calculates the vote surplus based on the elected / eliminated person's total vote weighings and quota
calculateVoteSurplus :: Float -> (String, Float) -> Float
calculateVoteSurplus quota removedFromNextRound = (snd removedFromNextRound) - quota

-- Calculates the new weighing based on the non-transferable and transferable vote counts
adjustWeighingToNewRanking :: Float -> [[String]] -> (String, Float) -> Float -> Float
adjustWeighingToNewRanking quota updatedVotes removedFromNextRound currentWeight =
  if transferableVoteCount <= surplus then currentWeight else currentWeight * (surplus / transferableVoteCount)
    where
      transferableVoteCount = changeIntToFloat $ length updatedVotes
      surplus = calculateVoteSurplus quota removedFromNextRound

-- Performs the single transferable vote operations all created above to generate STV results
getSTVResultSummary :: [[String]] -> [(Int, String)] -> Int -> Float -> Float -> Float -> [(String, [(Float, [String])])] -> Int -> [(Int, String, Float, Float, Float)]
getSTVResultSummary allVotes candidates 0 originalWeight currentWeight quota candidatesVotes roundNumber = []
getSTVResultSummary allVotes candidates seatCount originalWeight currentWeight quota candidatesVotes roundNumber =
  roundResult ++ getSTVResultSummary updatedVotes candidatesLeft updatedSeats originalWeight updatedWeight quota updatedCandidatesVotes (roundNumber + 1)
    where
      -- Calculating all of the necessary variables to determine the STV round results
      _updatedCandidatesVotes = updateCandidatesVotes candidatesVotes (filterVotesForEachCandidate allVotes candidates) currentWeight
      roundRanking = sortRanking $ countCandidatesVotes _updatedCandidatesVotes
      highestRanker = roundRanking !! 0
      pastQuota = (snd highestRanker > quota) || ((length candidates) <= seatCount)
      lowestRanker = roundRanking !! (length roundRanking - 1)
      removedFromNextRound = if pastQuota then highestRanker else lowestRanker
      candidatesLeft = removeFromCandidatesList candidates removedFromNextRound
      updatedVotes = redistributeVotes _updatedCandidatesVotes removedFromNextRound candidatesLeft
      updatedCandidatesVotes = removeFromCandidateVotes _updatedCandidatesVotes removedFromNextRound
      updatedSeats = if pastQuota then seatCount - 1 else seatCount
      updatedWeight = if pastQuota then adjustWeighingToNewRanking quota updatedVotes removedFromNextRound currentWeight else currentWeight
      
      -- Outputting the results for this round
      -- [(Round, Elected, Total Vote Weighing, Surplus, New Weighing)]
      roundResult =
        if pastQuota then
          [(roundNumber, fst removedFromNextRound, snd removedFromNextRound, calculateVoteSurplus quota removedFromNextRound, updatedWeight)]
        else
          []

-- Master vote results aggregation function used in Driver
-- Returns [(Round, Elected, Total Vote Weighing, Surplus, New Weighing)]
singleTransferableVote :: [[String]] -> [(Int, String)] -> Int -> Float -> [(Int, String, Float, Float, Float)]
singleTransferableVote cleanedVotes candidates seatCount weight =
  getSTVResultSummary cleanedVotes candidates seatCount weight weight quota [] 1
    where
      quota = getQuota cleanedVotes seatCount weight