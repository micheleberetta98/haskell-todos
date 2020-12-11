module Todo
  ( Todo
  , TodoStatus
  , newTodo
  , setDone
  , setNotDone
  , isDone
  ) where

import           Record (FromRecord (..), ToRecord (..))

----------------------- DATA
data TodoStatus = Done | NotDone deriving (Eq)
data Todo = Todo String TodoStatus

----------------------- INSTANCES
instance Show TodoStatus where
  show Done    = "✅"
  show NotDone = "❌"

instance Show Todo where
  show (Todo n s) = show s ++ " " ++ n

instance ToRecord Todo where
  toRecord (Todo n Done)    = "D " ++ n
  toRecord (Todo n NotDone) = "N " ++ n

instance FromRecord Todo where
  fromRecord ('D':' ':n) = Todo n Done
  fromRecord ('N':' ':n) = Todo n NotDone

----------------------- FUNCTIONS
newTodo :: String -> Todo
newTodo name = Todo name NotDone

setDone :: Todo -> Todo
setDone = updateState Done

setNotDone :: Todo -> Todo
setNotDone = updateState NotDone

updateState :: TodoStatus -> Todo -> Todo
updateState s' (Todo n _) = Todo n s'

isDone :: Todo -> Bool
isDone (Todo _ s) = s == Done
