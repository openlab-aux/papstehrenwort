{-# LANGUAGE FlexibleInstances #-}
module Papstehrenwort.Web.Html where

import qualified Papstehrenwort.Types as T
import Text.Blaze.Html5
import Text.Blaze (ToMarkup)

newtype Task = Task T.Task

instance ToMarkup [Task] where
  toMarkup t = toHtml "hello"
