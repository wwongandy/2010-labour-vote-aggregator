module Main where

import Lib
import Clean.CleanVotes
import Count.AlternativeVote
import Count.SingleTransferableVote

main :: IO ()
main = do
  ukVotes <- readFile "data/sampleuk5.csv"

  ------------------------------------------------------
  -- Generating results
  ------------------------------------------------------
  print "Candidates"
  print $ getCandidates ukVotes
  print "Cleaned Votes"
  print $ cleanVotes ukVotes
  print "Alternative Vote Winner"
  print $ alternativeVote (cleanVotes ukVotes)