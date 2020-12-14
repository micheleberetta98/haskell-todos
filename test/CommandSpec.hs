module CommandSpec (commandTests) where

import           Command    (Command (..), CommandResult (..), execute, parse)
import           Test.Hspec
import           TodoList

-- parse, execute, isQuit, isInvalid

commandTests = describe "Command" $ do
  describe "parse" $ do
    describe "when has valid input" $ do
      it "should parse `+ [str]` as an Add command" $ do
        parse "+ Test" `shouldBe` Add "Test"
      it "should parse `do [int]` as a Do command" $ do
        parse "do 0" `shouldBe` Do 0
      it "should parse `undo [int]` as an Undo command" $ do
        parse "undo 0" `shouldBe` Undo 0
      it "should parse `ls` as a ListAll command" $ do
        parse "ls" `shouldBe` ListAll
      it "should parse `ld` as a ListDone command" $ do
        parse "ld" `shouldBe` ListDone
      it "should parse `ln` as a ListNotDone command" $ do
        parse "ln" `shouldBe` ListNotDone
      it "should parse `q` as a Quit command" $ do
        parse "q" `shouldBe` Quit

    describe "when has invalid input" $ do
      it "should be invalid when parsing `+[str]`" $ do
        parse "+test" `shouldBe` Invalid "+test"
      it "should be invalid when parsing `+`" $ do
        parse "+" `shouldBe` Invalid "+"
      it "should be invalid when parsing `do0`" $ do
        parse "do0" `shouldBe` Invalid "do0"
      it "should be invalid when parsing `do`" $ do
        parse "do" `shouldBe` Invalid "do"
      it "should be invalid when parsing `undo0`" $ do
        parse "undo0" `shouldBe` Invalid "undo0"
      it "should be invalid when parsing `undo`" $ do
        parse "undo" `shouldBe` Invalid "undo"
      it "should be invalid with random strings" $ do
        parse "asd" `shouldBe` Invalid "asd"

  describe "execute" $ do
    it "should give `OkQuit \"Bye!\"` when quitting" $ do
      execute Quit empty `shouldBe` OkQuit "Bye!"
    it "should give `NotOk [str]` when given an invalid command" $ do
      let s = "random message"
      execute (Invalid s) empty `shouldBe` (NotOk $ "Invalid command `" ++ s ++ "`")

