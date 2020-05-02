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
  print "Candidates"
  print $ getCandidates ukVotes

  let cleanedVotes = cleanVotes ukVotes
  print "Cleaned Votes"
  print $ cleanedVotes

  print "Alternative Vote Winner"
  print $ alternativeVote cleanedVotes

  let seatCount = 3
  print "Seat Count for Single Transferable Vote (STV)"
  print $ seatCount

  let weight = 1000
  print "Weight of Votes for STV"
  print $ weight

  print "STV Quota"
  print $ getQuota cleanedVotes seatCount