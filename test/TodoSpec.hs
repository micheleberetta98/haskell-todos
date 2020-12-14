module TodoSpec (todoTests) where

import           Test.Hspec
import           Todo

todoTests :: SpecWith ()
todoTests = describe "Todo" $ do
  describe "newTodo" $ do
    it "should create a new todo with the given name" $ do
      let t = newTodo "name"
      name t `shouldBe` "name"

    it "should create a not done todo" $ do
      isDone (newTodo "") `shouldBe` False

  describe "name" $ do
    it "should get a todo's name" $ do
      let n = "name"
      name (newTodo n) `shouldBe` n

  describe "setDone" $ do
    it "should change a todo's status to done" $ do
      let t = setDone (newTodo "test")
      t `shouldSatisfy` isDone

  describe "setNotDone" $ do
    it "should set a todo's status to not done" $ do
      let t = setNotDone $ setDone $ newTodo "test"
      t `shouldNotSatisfy` isDone

