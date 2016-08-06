module Main where

import Protolude
import Test.Tasty

import TestScheduler (schedulerTests)

main = defaultMain $ testGroup "tests" [ schedulerTests ]
