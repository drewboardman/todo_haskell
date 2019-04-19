module Main where

-- import Seeder (seed)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           TodoController           (app)

main :: IO ()
main = withStdoutLogger $ \aplogger -> do
        let settings = setPort 8081 $ setLogger aplogger defaultSettings
        runSettings settings app
