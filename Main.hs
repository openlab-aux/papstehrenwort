module Main where

import Papstehrenwort.Web (app)
import Network.Wai.Handler.Warp (run)

main = run 8080 app
