{-# LANGUAGE RecordWildCards, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
module TestScheduler where

import Protolude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series (Serial, series, generate)
import Data.Time.Calendar
import Numeric.Natural
import GHC.Generics

import qualified Papstehrenwort.Scheduler as S

schedulerTests :: TestTree
schedulerTests = testGroup "scheduler tests"
  [ testGroup "next occurrence"
    [ todayBeforeStart
    , alwaysAfterStart
    , multipleOfRecur ]
  ]


data Occurrence = Occ { start :: Integer
                     , recur :: Natural
                     , today :: Integer } deriving (Show, Generic)
instance Monad m => Serial m Occurrence
instance Monad m => Serial m Natural where
  series = generate $ \n -> [1..(fromInteger $ toInteger n)]

nextOcc :: Occurrence -> Integer
nextOcc Occ{..} =
  let (ModifiedJulianDay next) =
        S.nextOccurrence (ModifiedJulianDay start) recur (ModifiedJulianDay today)
  in next

todayBeforeStart, alwaysAfterStart, multipleOfRecur :: TestTree

todayBeforeStart = testCase "if today is before start, the next occurrence should be start"
  $ nextOcc (Occ start 3 0) @?= start
  where start = 5


alwaysAfterStart = testProperty "the date is always after the start date"
  $ \o@Occ{..} -> nextOcc o >= start

multipleOfRecur = testProperty "the date is aways a multiple of recur from the start date"
  $ \o@Occ{..} -> today >= start ==>
  let next = nextOcc o
      rest = (next - start) `mod` toInteger recur
  in if rest == 0
    then Right "date is a multiple"
    else Left $ "nextOcc was "
         <> show (nextOcc o)
         <> " which is a mod of "
         <> show rest
