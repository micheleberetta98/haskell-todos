module TodoState
  ( TodoState
  , UpdateAction(..)
  , ListAction(..)
  , update
  , list
  ) where

import           Control.Monad.State (MonadState (get), State, gets, modify)
import           Todo                (isDone)
import qualified TodoList            as TL

----------------------- DATA
type TodoState = State TL.TodoList
data UpdateAction = AddTodo String | SetTodoDone Int | UnsetTodoDone Int
data ListAction = ListAllTodos | ListDoneTodos | ListNotDoneTodos

----------------------- FUNCTIONS
update :: UpdateAction -> TodoState ()
update (AddTodo name)    = modify $ TL.add name
update (SetTodoDone i)   = modify $ TL.doTodo i
update (UnsetTodoDone i) = modify $ TL.undoTodo i

list :: ListAction -> TodoState TL.TodoList
list ListAllTodos     = get
list ListDoneTodos    = gets $ TL.filter' isDone
list ListNotDoneTodos = gets $ TL.filter' (not . isDone)
