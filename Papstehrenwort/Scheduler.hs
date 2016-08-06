{-# LANGUAGE ViewPatterns #-}
module Papstehrenwort.Scheduler where

import Protolude
import Numeric.Natural
import Data.Time.Calendar (Day, diffDays, addDays)

data SchedState = SchedState { _today :: Day }

nextOccurrence :: Day     -- ^ starting date
               -> Natural -- ^ recurrence in days
               -> Day     -- ^ current date
               -> Day     -- ^ date of next occurence
nextOccurrence start (toInteger -> recur) today =
  if diff < 0
  then start
  else addDays (recur - (diff `mod` recur)) today
  where diff = diffDays today start
