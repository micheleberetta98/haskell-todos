import           Test.Hspec

import           CommandSpec  (commandTests)
import           TodoListSpec (todoListTests)
import           TodoSpec     (todoTests)


main :: IO ()
main = hspec $ do
  todoTests
  todoListTests
  commandTests
