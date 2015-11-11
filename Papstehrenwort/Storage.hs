{-# LANGUAGE OverloadedStrings #-}
module Papstehrenwort.Storage where

import Papstehrenwort.Types

import Database.HSparql.QueryGenerator

allTasks :: Query SelectQuery
allTasks = do
  pa <- prefix "pa" (iriRef "file:///home/lukas/Hacking/hs/papstehrenwort/example-data.n3#pa")
  task <- var
  triple task (iriRef "a") (pa .:. "Task")
  return SelectQuery { queryVars = [task] }

allUsers :: Query SelectQuery
allUsers = do
  foaf <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
  user <- var
  triple user (iriRef "a") (foaf .:. "Person")
  return SelectQuery { queryVars = [user] }
