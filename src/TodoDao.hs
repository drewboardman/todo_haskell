{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module TodoDao
  ( Todo
  , TodoT(Todo)
  , TodoDb
  , _todos
  , todoDb
  )
where

import qualified Data.Text            as T
import           Data.Time.Clock      (UTCTime)
import           Database.Beam        (Beamable, Columnar, Database,
                                       DatabaseSettings, Generic, Identity,
                                       PrimaryKey,
                                       Table (PrimaryKey, primaryKey),
                                       TableEntity, defaultDbSettings)
import           Database.Beam.Sqlite (Sqlite)

type Todo = TodoT Identity

instance Beamable TodoT

instance Table TodoT where
  data PrimaryKey TodoT f = TodoTableID (Columnar f String)
    deriving (Generic, Beamable)
  primaryKey = TodoTableID . _todoId

data TodoT f = Todo
  { _todoId        :: Columnar f String
  , _todoContent   :: Columnar f T.Text
  , _todoCreatedAt :: Columnar f UTCTime
  , _todoIsPending :: Columnar f Bool } deriving (Generic)

data TodoDb f = TodoDb
  { _todos :: f (TableEntity TodoT) } deriving (Generic, Database Sqlite)

todoDb :: DatabaseSettings be TodoDb
todoDb = defaultDbSettings
