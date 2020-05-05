module Main where

import Lib
import Clean.CleanVotes
import Count.AlternativeVote
import Count.SingleTransferableVote

main :: IO ()
main = do
  ukVotes <- readFile "data/uk.csv"

  ------------------------------------------------------
  -- Generating results
  ------------------------------------------------------
  let candidates = getCandidates ukVotes
  print "Candidates"
  print $ candidates

  let cleanedVotes = cleanVotes ukVotes
  print "Cleaned Votes"
  print $ cleanedVotes

  print "Alternative Vote Winner"
  print $ alternativeVote cleanedVotes

  let seatCount = 4
  print "Seat Count for Single Transferable Vote (STV)"
  print $ seatCount

  let weight = changeIntToFloat 1
  print "Weight of Votes for STV"
  print $ weight

  let quota = getQuota cleanedVotes seatCount weight
  print "STV Quota"
  print $ quota

  print "STV Results"
  -- let updatedVotes = addVotesToRanking [] cleanedVotes candidates 1
  -- print $ updatedVotes

  print $ singleTransferableVote cleanedVotes candidates seatCount weight
  -- let updatedRanking = sortRanking $ addRankings (getCurrentRoundRanking cleanedVotes candidates) []
  -- print $ updatedRanking
  -- let highestRanking = updatedRanking !! 0
  -- print $ highestRanking
  -- let lowestRanking = updatedRanking !! (length (updatedRanking) - 1)
  -- print $ lowestRanking
  -- let pastQuota = changeIntToFloat (length (snd highestRanking)) * weight >= quota
  -- print $ pastQuota
  -- let removedFromContest = if pastQuota then highestRanking else lowestRanking
  -- print $ removedFromContest
  -- let updatedCandidates = removeCandidateFromListing candidates removedFromContest
  -- print $ updatedCandidates
  -- let updatedVotes = redistributeVotesFromCandidate updatedRanking removedFromContest
  -- print $ updatedVotes
  -- let updatedWeight = if pastQuota then adjustWeightingToSurplus weight quota removedFromContest updatedVotes else weight
  -- print $ updatedWeight