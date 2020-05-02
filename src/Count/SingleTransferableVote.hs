module Count.SingleTransferableVote where

------------------------------------------------------
-- Algorithm 2: Single Transferable Vote
------------------------------------------------------

-- Master quota calculator function used in Driver
getQuota :: [[String]] -> Int -> Int
getQuota cleanedVotes seatCount = (length cleanedVotes `div` (seatCount + 1)) + 1