module Count.SingleTransferableVote where

import Clean.CleanVotes
import Debug.Trace

------------------------------------------------------
-- Algorithm 2: Single Transferable Vote
------------------------------------------------------

-- Master quota calculator function used in Driver
getQuota :: [[String]] -> Int -> Float -> Float
getQuota cleanedVotes _seatCount weight =
  ((validVotes / (seatCount + 1)) + 1) * weight
    where
      validVotes = changeIntToFloat $ length cleanedVotes
      seatCount = changeIntToFloat $ _seatCount

-- Changes an int value to a float
changeIntToFloat :: Int -> Float
changeIntToFloat x = fromIntegral x :: Float

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
updateCandidatesVotes updatedCandidatesVotes [] currentWeight =
  updatedCandidatesVotes
updateCandidatesVotes updatedCandidatesVotes _candidatesVotes currentWeight =
  [ (x, y ++ b) | (x, y) <- updatedCandidatesVotes, (a, b) <- candidatesVotes, x == a ]
    where
      candidatesVotes = updateCandidatesVotes [] _candidatesVotes currentWeight

-- Adds all of the numbers together from a list
addAll :: (Num a) => [a] -> a
addAll (x : []) = x
addAll (x : xs) = x + addAll xs

-- Weighs up all of the votes for each candidate to produce a ranking
weighCandidateVotes :: [(String, [(Float, [String])])] -> [(String, Float)]
weighCandidateVotes candidatesVotes =
  map (\x -> (fst x, addAll [y | (y, _) <- snd x ]) ) candidatesVotes

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
removeFromCandidatesList candidates removedFromNextRound =
  filter (\x -> snd x /= candidate) candidates
    where
      candidate = fst removedFromNextRound

-- Removes the given candidate from the list of candidate votes
removeFromCandidateVotes :: [(String, [(Float, [String])])] -> (String, Float) -> [(String, [(Float, [String])])]
removeFromCandidateVotes candidatesVotes removedFromNextRound =
  filter (\x -> fst x /= candidate) candidatesVotes
    where
      candidate = fst removedFromNextRound

-- Filters out the first invalid candidate from a vote
filterInvalidCandidatesFromVote :: [String] -> [String] -> [String]
filterInvalidCandidatesFromVote [] candidates = []
filterInvalidCandidatesFromVote (x : vote) candidates
  | x `elem` candidates = x : vote
  | otherwise           = filterInvalidCandidatesFromVote vote candidates

-- Ensures that atleast a single vote belongs to a candidate in the given listing
ensureVoteForCandidates :: [String] -> [String] -> Bool
ensureVoteForCandidates vote candidates = (length $ filteredVote) > 0
  where
    filteredVote = filterInvalidCandidatesFromVote vote candidates

-- Gets the elected or eliminated candidate's vote body for this round
getCandidateVotes :: [(String, [(Float, [String])])] -> (String, Float) -> (String, [(Float, [String])])
getCandidateVotes candidatesVotes removedFromNextRound =
  (filter (\x -> fst x == candidate) candidatesVotes) !! 0
    where
      candidate = fst removedFromNextRound

-- Redistributes all of the votes belonging to a candidate to the master vote listing
redistributeVotes :: [(String, [(Float, [String])])] -> (String, Float) -> [(Int, String)] -> [[String]]
redistributeVotes candidatesVotes removedFromNextRound _candidatesLeft =
  [ (filterInvalidCandidatesFromVote b candidatesLeft) | (a, b) <- snd candidateVotes, length b > 1, ensureVoteForCandidates b candidatesLeft ]
    where
      candidateVotes = getCandidateVotes candidatesVotes removedFromNextRound
      candidatesLeft = secondElementToList _candidatesLeft

-- Weighs up all the transferable votes from a candidate
weighTransferableVotesFromCandidate :: [(String, [(Float, [String])])] -> (String, Float) -> [(Int, String)] -> Float
weighTransferableVotesFromCandidate candidatesVotes removedFromNextRound _candidatesLeft =
  addAll [ a | (a, b) <- snd candidateVotes, ensureVoteForCandidates b candidatesLeft ]
    where
      candidateVotes = getCandidateVotes candidatesVotes removedFromNextRound
      candidatesLeft = secondElementToList _candidatesLeft

-- Calculates the vote surplus based on the elected or eliminated person's total vote weighings and quota
getVoteSurplus :: Float -> (String, Float) -> Float
getVoteSurplus quota removedFromNextRound = (snd removedFromNextRound) - quota

-- Calculates the new weighing based on the non-transferable and transferable vote counts
adjustWeighingToNewRanking :: Float -> Float -> (String, Float) -> Float -> Float -> Float
adjustWeighingToNewRanking quota transferableVoteWeight removedFromNextRound originalWeight currentWeight =
  if transferableVoteWeight <= surplus then
    currentWeight
  else
    -- trace ("surplus: " ++ show surplus ++ " | transferableVoteWeight: " ++ show transferableVoteWeight ++ " | currentWeight: " ++ show currentWeight)
    originalWeight * (surplus / transferableVoteWeight)  
  where
    surplus = getVoteSurplus quota removedFromNextRound

-- Performs the single transferable vote operations all created above to generate STV results
getSTVResultSummary :: [[String]] -> [(Int, String)] -> Int -> Float -> Float -> Float -> [(String, [(Float, [String])])] -> Int -> [(Int, String, Float, Float, Float)]
getSTVResultSummary allVotes candidates 0 originalWeight currentWeight quota candidatesVotes roundNumber = []
getSTVResultSummary allVotes candidates seatCount originalWeight currentWeight quota candidatesVotes roundNumber =
  roundResult ++ getSTVResultSummary updatedVotes candidatesLeft updatedSeats originalWeight updatedWeight quota updatedCandidatesVotes (roundNumber + 1)
    where
      -- Calculating all of the necessary variables to determine the STV round results
      _updatedCandidatesVotes = updateCandidatesVotes candidatesVotes (filterVotesForEachCandidate allVotes candidates) currentWeight
      roundRanking = sortRanking $ weighCandidateVotes _updatedCandidatesVotes
      highestRanker = roundRanking !! 0
      pastQuota = (snd highestRanker > quota) || ((length candidates) <= seatCount)
      lowestRanker = roundRanking !! (length roundRanking - 1)
      removedFromNextRound = if pastQuota then highestRanker else lowestRanker
      candidatesLeft = removeFromCandidatesList candidates removedFromNextRound
      updatedVotes = redistributeVotes _updatedCandidatesVotes removedFromNextRound candidatesLeft
      updatedCandidatesVotes = removeFromCandidateVotes _updatedCandidatesVotes removedFromNextRound
      updatedSeats = if pastQuota then seatCount - 1 else seatCount
      transferableVoteWeight = weighTransferableVotesFromCandidate _updatedCandidatesVotes removedFromNextRound candidatesLeft
      updatedWeight = if pastQuota then adjustWeighingToNewRanking quota transferableVoteWeight removedFromNextRound originalWeight currentWeight else currentWeight
      
      -- Outputting the results for this round
      -- [(Round, Elected, Total Vote Weighing, Surplus, New Weighing)]
      roundResult =
        if pastQuota then
          [(roundNumber, fst removedFromNextRound, snd removedFromNextRound, getVoteSurplus quota removedFromNextRound, updatedWeight)]
        else
          []

-- Master vote results aggregation function used in Driver
-- Returns [(Round, Elected, Total Vote Weighing, Surplus, New Weighing)]
singleTransferableVote :: [[String]] -> [(Int, String)] -> Int -> Float -> [(Int, String, Float, Float, Float)]
singleTransferableVote cleanedVotes candidates seatCount weight =
  getSTVResultSummary cleanedVotes candidates seatCount weight weight quota [] 1
    where
      quota = getQuota cleanedVotes seatCount weight