module TodoListSpec (todoListTests) where

import           Test.Hspec
import           Todo
import           TodoList


todoListTests :: SpecWith ()
todoListTests = describe "TodoList" $ do
  describe "empty" $ do
    it "should be an empty list" $ do
      count empty `shouldBe` 0

  describe "count" $ do
    it "should return the correct list length" $ do
      let
        l0 = empty
        l1 = add "name" l0
        l3 = add "name" (add "name" l1)
      count l0 `shouldBe` 0
      count l1 `shouldBe` 1
      count l3 `shouldBe` 3

  describe "add" $ do
    it "should add a new todo" $ do
      let
        l0 = empty
        l1 = add "name" l0
      count l1 `shouldBe` count l0 + 1

  describe "doTodo" $ do
    it "should set done a todo with a valid id" $ do
      let
        l = add "0" (add "1" (add "2" empty))
        l' = doTodo 1 l
        (Just t) = get 1 l'
      t `shouldSatisfy` Todo.isDone

    it "should do nothing if passed an invalid id" $ do
      let
        l = add "0" (add "1" (add "2" empty))
        l' = doTodo 100 l
      l `shouldBe` l'

  describe "undoTodo" $ do
    it "should set not done a todo with a valid id" $ do
      let
        l = add "0" (add "1" (add "2" empty))
        l' = undoTodo 1 (doTodo 1 l)
        (Just t) = get 1 l'
      t `shouldNotSatisfy` Todo.isDone

    it "should do nothing if passed an invalid id" $ do
      let
        l = add "0" (add "1" (add "2" empty))
        l' = undoTodo 100 l
      l `shouldBe` l'

  describe "filterTodos" $ do
    let
      l = add "0" (add "1" (add "2" empty))
      l' = doTodo 1 l
    it "should filter done todos" $ do
      let filtered = filterTodos Todo.isDone l'
      count filtered `shouldBe` 1
    it "should filter not done todos" $ do
      let filtered = filterTodos (not . Todo.isDone) l'
      count filtered `shouldBe` 2
