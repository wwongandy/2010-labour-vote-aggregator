module Count.SingleTransferableVote where

------------------------------------------------------
-- Algorithm 2: Single Transferable Vote
------------------------------------------------------

changeIntToFloat :: Int -> Float
changeIntToFloat x = fromIntegral x :: Float

-- Master quota calculator function used in Driver
getQuota :: [[String]] -> Int -> Float -> Float
getQuota cleanedVotes seatCount weight = (changeIntToFloat (length cleanedVotes) / (changeIntToFloat (seatCount) + 1) + 1) * weight

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

-- Takes the first element of two tuples to build a list of them
firstElementToList :: [(a, b)] -> [a]
firstElementToList = map (fst)

redistributeVotesFromCandidate :: [(String, [[String]])] -> (String, [[String]]) -> [[String]]
redistributeVotesFromCandidate updatedRanking removedFromContest =
  map (tail) $ filter (\xs -> (length xs > 1) && xs !! 1 `elem` candidatesLeft) $ snd removedFromContest
    where
      candidatesLeft = firstElementToList updatedRanking

adjustWeightingToSurplus :: Float -> Float -> (String, [[String]]) -> [[String]] -> Float
adjustWeightingToSurplus originalWeight quota removedFromContest updatedVotes = originalWeight * (surplus / nonTransferableVoteCount)
  where
    nonTransferableVoteCount = originalWeight * changeIntToFloat (length $ updatedVotes)
    transferableVoteCount = originalWeight * changeIntToFloat (length $ snd removedFromContest)
    surplus = transferableVoteCount - quota

-- Master vote results calculation function used in Driver
singleTransferableVote :: [[String]] -> [(Int, String)] -> Int -> Float -> [String]
singleTransferableVote cleanedVotes candidates seatCount weight =
  electCandidatesSTV cleanedVotes candidates seatCount weight weight quota []
    where
      quota = getQuota cleanedVotes seatCount weight

electCandidatesSTV :: [[String]] -> [(Int, String)] -> Int -> Float -> Float -> Float -> [(String, [[String]])] -> [String]
electCandidatesSTV cleanedVotes candidates seatCount originalWeight weight quota currentRanking
  | seatCount > 0   = if pastQuota then [fst removedFromContest] else [] ++ electCandidatesSTV updatedVotes updatedCandidates updatedSeatCount originalWeight updatedWeight quota updatedRanking
  | otherwise       = []
  where
    updatedRanking = sortRanking $ addRankings (getCurrentRoundRanking cleanedVotes candidates) currentRanking
    highestRanking = updatedRanking !! 0
    lowestRanking = updatedRanking !! (length (updatedRanking) - 1)
    pastQuota = (changeIntToFloat $ length (snd highestRanking)) * weight >= quota || length updatedRanking <= seatCount
    removedFromContest = if pastQuota then highestRanking else lowestRanking
    updatedCandidates = removeCandidateFromListing candidates removedFromContest
    updatedVotes = redistributeVotesFromCandidate updatedRanking removedFromContest
    updatedWeight = if pastQuota then adjustWeightingToSurplus originalWeight quota removedFromContest updatedVotes else originalWeight
    updatedSeatCount = if pastQuota then seatCount - 1 else seatCount