module Papstehrenwort.Scheduler where

import Protolude
import Numeric.Natural
import Data.Time.Calendar (Day, diffDays, addDays)

data SchedState = SchedState { _today :: Day }

nextOccurrence :: Day     -- ^ starting date
               -> Natural -- ^ recurrence in days
               -> Day     -- ^ current date
               -> Day     -- ^ date of next occurence
nextOccurrence start recur today =
  addDays ((diffDays today start) `mod` toInteger recur) today
