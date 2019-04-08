{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module TodoDao
  ( Todo
  , TodoT(Todo)
  )
where

import           Data.Time.Clock      (UTCTime)
import           Database.Beam        (Beamable, Columnar, Database,
                                       DatabaseSettings, Generic, Identity,
                                       PrimaryKey, Table (..), TableEntity,
                                       defaultDbSettings)
import           Database.Beam.Sqlite
import           Models               (Content)

type Todo = TodoT Identity

instance Beamable TodoT

instance Table TodoT where
  data PrimaryKey TodoT f = TodoTableID (Columnar f UTCTime)
    deriving (Generic, Beamable)
  primaryKey = TodoTableID . _createdAt

-- figure out how to deal with the IO monad in Time.getCurrentTime
data TodoT f = Todo
  { _content   :: Columnar f Content
  , _createdAt :: Columnar f UTCTime
  , _isPending :: Columnar f Bool } deriving (Generic)

newtype TodoDb f = TodoDb
  { _todos :: f (TableEntity TodoT) } deriving (Generic, Database Sqlite)

todoDb :: DatabaseSettings be TodoDb
todoDb = defaultDbSettings
