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
  print $ singleTransferableVote cleanedVotes candidates seatCount weight