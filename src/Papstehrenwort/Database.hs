{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, TypeFamilies, GADTs, GeneralizedNewtypeDeriving #-}
module Papstehrenwort.Database
   where

import Protolude
import Data.Time.Calendar (Day)

import Database.Persist.TH

import Papstehrenwort.Types

mkPersist sqlSettings [persistLowerCase|
  DBTask sql=task
    title Text
    description Text
    how Text Maybe
    url Text Maybe
    recur Int
    start Day
    deriving Show Eq
  DBSplasher sql=splasher
    nick Text
    mail Text
    deriving Show Eq
|]

-- we need these for minimal serialization
-- also because we don’t want persistent to generate our logical
-- datatypes for us.
-- meh, boilerplate

taskToDb :: Task -> DBTask
taskToDb (Task tit desc how url recur start) =
  DBTask tit desc how (toS . exportURL <$> url) (fromInteger recur) start

dbToTask :: DBTask -> Task
dbToTask (DBTask tit desc how url recur start) =
  Task tit desc how (importURL =<< url) (toInteger recur) start

splasherToDb :: Splasher -> DBSplasher
splasherToDb (Splasher nick mail) = DBSplasher nick mail

dbToSplasher :: DBSplasher -> Splasher
dbToSplasher (DBSplasher nick mail) = Splasher nick mail
