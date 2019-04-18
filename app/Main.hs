module Main where

import Seeder (seed)
import TodoController (app)
import Network.Wai.Handler.Warp

main :: IO ()
main = seed
-- main = run 8081 app
