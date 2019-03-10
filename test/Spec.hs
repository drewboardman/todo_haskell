{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Time.Clock as Time
import Test.Hspec
import Models
import Data.Text

main :: IO ()
main = hspec $ do
  describe "creating a todo"  $ do
    it "should return a todo" $ do
      let text = pack "Hello World"
      todo <- newTodo $ text
      let Content inner = _content todo
      inner `shouldBe` text
  describe "completing a todo" $ do
    it "should create a completed todo" $ do
      todo <- newTodo $ pack "foo"
      let stamp = _finishedAt todo
      current <- Time.getCurrentTime
      stamp `shouldBe` current
