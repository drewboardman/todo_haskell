{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module TodoDao
  ( TodoT(Todo)
  , _todos
  , todoDb
  , allTodos
  , getSingleTodo
  , Todo
  , insertPendingTodo
  )

where

import           Control.Lens                             ((^.))
import           Data.Maybe                               (listToMaybe)
import qualified Data.Text                                as T
import           Data.Time.Clock                          (UTCTime)
import           Data.UUID                                (UUID, toText)
import           Database.Beam                            (Beamable, Columnar,
                                                           Database,
                                                           DatabaseSettings,
                                                           Generic, Identity,
                                                           LensFor (LensFor),
                                                           PrimaryKey,
                                                           Table (PrimaryKey, primaryKey),
                                                           TableEntity, all_,
                                                           defaultDbSettings,
                                                           guard_, insert,
                                                           insertValues,
                                                           runSelectReturningList,
                                                           select, val_, (==.))
import qualified Database.Beam.Backend.SQL.BeamExtensions as Extensions (runInsertReturningList)
import           Database.Beam.Schema                     (tableLenses)
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

getSingleTodo :: T.Text -> IO (Maybe Todo)
getSingleTodo uuid = do
  conn <- open "todo1.db"
  let resultAsList :: IO [Todo] = runBeamSqlite conn $ runSelectSingleUuid uuid
  fmap listToMaybe resultAsList

runSelectAll :: SqliteM [Todo]
runSelectAll = do
  let allTodosQuery = all_ (_todos todoDb)
  todos :: [Todo] <- runSelectReturningList $ select allTodosQuery
  return todos

-- SELECT * WHERE UUID=<uuid>
runSelectSingleUuid :: T.Text -> SqliteM [Todo]
runSelectSingleUuid uuid =
  runSelectReturningList $ select $ do
    let Todo (LensFor todoId) _ _ _ = tableLenses
    todo <- all_ $ _todos todoDb
    guard_ $ (todo ^. todoId) ==. val_ uuid
    return todo

insertPendingTodo :: T.Text -> UTCTime -> UUID -> IO (Maybe Todo)
insertPendingTodo content created uuid = do
  let myId = toText uuid
  conn <- open "todo1.db"
  let resultAsList = runBeamSqlite conn $
        Extensions.runInsertReturningList $
        insert (_todos todoDb) $
        insertValues [ Todo myId content created Nothing ]
  fmap listToMaybe resultAsList
