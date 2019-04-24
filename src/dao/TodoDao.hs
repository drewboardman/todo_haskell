{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module TodoDao ( TodoT(Todo)
               , _todos
               , todoDb
               , allTodos
               , completePendingTodo
               , Todo
               , insertPendingTodo
               , selectSingleTodo
               , updateTodo
               ) where

import           Data.Maybe                               (listToMaybe)
import qualified Data.Text                                as T
import           Data.Time.Clock                          (UTCTime)
import qualified Data.Time.Clock                          as Time
import           Data.UUID                                (UUID, toText)
import           Database.Beam                            (Beamable, Columnar,
                                                           Database,
                                                           DatabaseSettings,
                                                           Generic, Identity,
                                                           PrimaryKey,
                                                           Table (PrimaryKey, primaryKey),
                                                           TableEntity, all_,
                                                           defaultDbSettings,
                                                           insert, insertValues,
                                                           lookup_,
                                                           runSelectReturningList,
                                                           runSelectReturningOne,
                                                           runUpdate, save,
                                                           select)
import qualified Database.Beam.Backend.SQL.BeamExtensions as Extensions (runInsertReturningList)
import           Database.Beam.Sqlite                     (Sqlite, SqliteM,
                                                           runBeamSqlite)
import           Database.SQLite.Simple                   (open)

type Todo = TodoT Identity

instance Beamable TodoT

instance Table TodoT where
  data PrimaryKey TodoT f = TodoTableID (Columnar f T.Text)
    deriving (Generic, Beamable)
  primaryKey = TodoTableID . _todoId

data TodoT f = Todo
  { _todoId         :: Columnar f T.Text
  , _todoContent    :: Columnar f T.Text
  , _todoCreatedAt  :: Columnar f UTCTime
  , _todoFinishedAt :: Columnar f (Maybe UTCTime) } deriving (Generic)

newtype TodoDb f = TodoDb
  { _todos :: f (TableEntity TodoT) } deriving (Generic, Database Sqlite)

todoDb :: DatabaseSettings be TodoDb
todoDb = defaultDbSettings

allTodos :: IO [Todo]
allTodos = do
  conn <- open "todo1.db"
  runBeamSqlite conn runSelectAll

runSelectAll :: SqliteM [Todo]
runSelectAll = do
  let allTodosQuery = all_ (_todos todoDb)
  todos :: [Todo] <- runSelectReturningList $ select allTodosQuery
  return todos

insertPendingTodo :: T.Text -> UTCTime -> UUID -> IO (Maybe Todo)
insertPendingTodo content created uuid = do
  let myId = toText uuid
  conn <- open "todo1.db"
  let resultAsList = runBeamSqlite conn $
        Extensions.runInsertReturningList $
        insert (_todos todoDb) $
        insertValues [ Todo myId content created Nothing ]
  fmap listToMaybe resultAsList

updateTodo :: UUID -> T.Text -> IO (Maybe Todo)
updateTodo uuid updatedText = do
  conn <- open "todo1.db"
  Just todo <- selectSingleTodo uuid
  runBeamSqlite conn $ runUpdate $
    save (_todos todoDb)
    (todo { _todoContent = updatedText })
  selectSingleTodo uuid

selectSingleTodo :: UUID -> IO (Maybe Todo)
selectSingleTodo uuid = do
  conn <- open "todo1.db"
  let id' = toText uuid
  runBeamSqlite conn $
    runSelectReturningOne $
    lookup_ (_todos todoDb) (TodoTableID id')

completePendingTodo :: UUID -> IO (Maybe Todo)
completePendingTodo uuid = do
  conn <- open "todo1.db"
  Just todo <- selectSingleTodo uuid
  time <- Time.getCurrentTime
  runBeamSqlite conn $ runUpdate $
    save (_todos todoDb)
    (todo { _todoFinishedAt = pure time })
  selectSingleTodo uuid
