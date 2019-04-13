{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Seeder (seed) where

import qualified Data.Time.Clock                 as Time (getCurrentTime)
import           Data.UUID                       (toText)
import qualified Data.UUID.V4                    as Uuid (nextRandom)
import           Database.Beam                   (insert, insertValues,
                                                  runInsert)
import           Database.Beam.Sqlite.Connection (runBeamSqlite)
import           Database.SQLite.Simple          (open)
import           Models                          (Content(Content))
import qualified TodoDao                         as Dao (TodoT (..), todoDb,
                                                         _todos)

seed :: IO ()
seed = do
  let Content content = Content "get this to compile"
  myId <- toText <$> Uuid.nextRandom
  now <- Time.getCurrentTime
  conn <- open "todo1.db"
  runBeamSqlite conn $ runInsert $
    insert (Dao._todos Dao.todoDb) $
      insertValues [ Dao.Todo myId content now True ]
