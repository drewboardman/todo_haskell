{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Text
import qualified Data.Time.Clock as Time
import qualified Models.Completed as C
import qualified Models.Pending as P
import           Models          (Content (..), completePendingTodo, newTodo)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "creating a todo"  $ do
   it "should return a todo" $ do
      let text = pack "Hello World"
      todo <- newTodo text
      let Content inner = P._content todo
      inner `shouldBe` text
  describe "completing a todo" $ do
    it "should create a completed todo" $ do
      todo <- newTodo $ pack "foo"
      stamp <- _finishedAt <$> completePendingTodo todo
      current <- Time.getCurrentTime
      stamp `shouldBe` current
