{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

-- module TodoDao (Todo, TodoT) where
module TodoDao
  ( Todo
  , TodoT(Todo)
  ) where

import           Database.Beam (Columnar, Generic, Identity, PrimaryKey)
import           Models        (Content)

type Todo = TodoT Identity
type TodoTableID = PrimaryKey TodoT Identity

-- figure out how to deal with the IO monad in Time.getCurrentTime
data TodoT f = Todo
  { _content   :: Columnar f Content
  , _isPending :: Columnar f Bool } deriving (Generic)
