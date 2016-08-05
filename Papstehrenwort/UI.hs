{-|
Module      : Papstehrenwort.UI
Description : Representation of the Papstehrenwort UI
-}
{-# LANGUAGE TemplateHaskell, NoImplicitPrelude #-}
module Papstehrenwort.UI
  ( Table, table, tableHead, tableBody ) where

import Protolude
import Control.Lens.TH

data Table a = Table { _tableHead :: [Text]
                     , _tableBody :: [a] }

table :: [a] -> Table a
table a = Table { _tableHead = [], _tableBody = a }

makeLenses ''Table
