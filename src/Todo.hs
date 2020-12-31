module Todo
  ( Todo
  , TodoStatus
  , name
  , newTodo
  , setDone
  , setNotDone
  , isDone
  ) where

import           TodoStatus (TodoStatus (..))

import           Prettify   (Pretty (..))
import           Record     (FromRecord (..), ToRecord (..))

----------------------- DATA
data Todo = Todo String TodoStatus deriving (Show, Eq)

----------------------- INSTANCES
instance Pretty Todo where
  prettify (Todo n s) = prettify s ++ " " ++ n

instance ToRecord Todo where
  toRecord (Todo n Done)    = "D;" ++ n
  toRecord (Todo n NotDone) = "N;" ++ n

instance FromRecord Todo where
  fromRecord ('D':';':n) = Just (Todo n Done)
  fromRecord ('N':';':n) = Just (Todo n NotDone)
  fromRecord _           = Nothing

----------------------- FUNCTIONS
newTodo :: String -> Todo
newTodo name = Todo name NotDone

name :: Todo -> String
name (Todo n _) = n

setDone :: Todo -> Todo
setDone = updateState Done

setNotDone :: Todo -> Todo
setNotDone = updateState NotDone

updateState :: TodoStatus -> Todo -> Todo
updateState s (Todo n _) = Todo n s

isDone :: Todo -> Bool
isDone (Todo _ s) = s == Done
