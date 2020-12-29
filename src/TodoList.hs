module TodoList
  ( TodoList
  , empty
  , add
  , doTodo
  , get
  , undoTodo
  , count
  , filter'
) where

import           Prettify (Pretty (..))
import           Record (FromRecord (..), ToRecord (..))
import           Todo   (Todo, newTodo, setDone, setNotDone)

----------------------- DATA
newtype TodoList = TodoList [Todo] deriving (Show, Eq)

----------------------- INSTANCES
instance Pretty TodoList where
  prettify (TodoList tl) = unlines $ zipWith prettify' [0..] tl
    where prettify' i t = show i ++ ". " ++ prettify t

instance ToRecord TodoList where
  toRecord (TodoList tl) = unlines $ map toRecord tl

instance FromRecord TodoList where
  fromRecord = TodoList . map fromRecord . filter (/="") . lines

----------------------- FUNCTIONS
empty :: TodoList
empty = TodoList []

count :: TodoList -> Int
count (TodoList tl) = length tl

get :: Int -> TodoList -> Maybe Todo
get i (TodoList tl)
  | i >= length tl = Nothing
  | otherwise     = Just $ tl !! i

add :: String -> TodoList -> TodoList
add n (TodoList tl) = TodoList (newTodo n : tl)

doTodo :: Int -> TodoList -> TodoList
doTodo = applyToTodo setDone

undoTodo :: Int -> TodoList -> TodoList
undoTodo = applyToTodo setNotDone

applyToTodo :: (Todo -> Todo) -> Int -> TodoList -> TodoList
applyToTodo f i (TodoList tl) = TodoList $ zipWith changeState [0..] tl
  where
    changeState j
      | j == i    = f
      | otherwise = id

filter' :: (Todo -> Bool) -> TodoList -> TodoList
filter' f (TodoList tl) = TodoList (filter f tl)
