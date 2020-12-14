import           Test.Hspec
import qualified Todo
import qualified TodoList

main :: IO ()
main = hspec $ do
  todoTests
  todoListTests

todoTests :: SpecWith ()
todoTests = describe "Todo" $ do
  describe "newTodo" $ do
    it "should create a new todo with the given name" $ do
      let t = Todo.newTodo "name"
      Todo.name t `shouldBe` "name"

    it "should create a not done todo" $ do
      Todo.isDone (Todo.newTodo "") `shouldBe` False

  describe "name" $ do
    it "should get a todo's name" $ do
      let n = "name"
      Todo.name (Todo.newTodo n) `shouldBe` n

  describe "setDone" $ do
    it "should change a todo's status to done" $ do
      let t = Todo.setDone (Todo.newTodo "test")
      t `shouldSatisfy` Todo.isDone

  describe "setNotDone" $ do
    it "should set a todo's status to not done" $ do
      let t = Todo.setNotDone $ Todo.setDone $ Todo.newTodo "test"
      t `shouldNotSatisfy` Todo.isDone

todoListTests :: SpecWith ()
todoListTests = describe "TodoList" $ do
  describe "empty" $ do
    it "should be an empty list" $ do
      TodoList.count TodoList.empty `shouldBe` 0

  describe "count" $ do
    it "should return the correct list length" $ do
      let
        l0 = TodoList.empty
        l1 = TodoList.add "name" l0
        l3 = TodoList.add "name" (TodoList.add "name" l1)
      TodoList.count l0 `shouldBe` 0
      TodoList.count l1 `shouldBe` 1
      TodoList.count l3 `shouldBe` 3

  describe "add" $ do
    it "should add a new todo" $ do
      let
        l0 = TodoList.empty
        l1 = TodoList.add "name" l0
      TodoList.count l1 `shouldBe` TodoList.count l0 + 1

  describe "doTodo" $ do
    it "should set done a todo with a valid id" $ do
      let
        l = TodoList.add "0" (TodoList.add "1" (TodoList.add "2" TodoList.empty))
        l' = TodoList.doTodo 1 l
        (Just t) = TodoList.get 1 l'
      t `shouldSatisfy` Todo.isDone

    it "should do nothing if passed an invalid id" $ do
      let
        l = TodoList.add "0" (TodoList.add "1" (TodoList.add "2" TodoList.empty))
        l' = TodoList.doTodo 100 l
      l `shouldBe` l'

  describe "undoTodo" $ do
    it "should set not done a todo with a valid id" $ do
      let
        l = TodoList.add "0" (TodoList.add "1" (TodoList.add "2" TodoList.empty))
        l' = TodoList.undoTodo 1 (TodoList.doTodo 1 l)
        (Just t) = TodoList.get 1 l'
      t `shouldNotSatisfy` Todo.isDone

    it "should do nothing if passed an invalid id" $ do
      let
        l = TodoList.add "0" (TodoList.add "1" (TodoList.add "2" TodoList.empty))
        l' = TodoList.undoTodo 100 l
      l `shouldBe` l'

  describe "filter'" $ do
    let
      l = TodoList.add "0" (TodoList.add "1" (TodoList.add "2" TodoList.empty))
      l' = TodoList.doTodo 1 l
    it "should filter done todos" $ do
      let filtered = TodoList.filter' Todo.isDone l'
      TodoList.count filtered `shouldBe` 1
    it "should filter not done todos" $ do
      let filtered = TodoList.filter' (not . Todo.isDone) l'
      TodoList.count filtered `shouldBe` 2
