{-# LANGUAGE FlexibleInstances #-}

module TodoList
  ( TodoList
  , empty
  , add
  , doTodo
  , get
  , undoTodo
  , count
  , filterTodos
) where

import           Data.Bifunctor (bimap, second)
import qualified Data.Map       as M
import           Data.Maybe     (fromMaybe, mapMaybe)
import           Prettify       (Pretty (..))
import           Record         (FromRecord (..), ToRecord (..))
import           StringUtils
import           Todo           (Todo, newTodo, setDone, setNotDone)

----------------------- DATA
type TodoList = M.Map Int Todo

----------------------- INSTANCES
instance Pretty TodoList where
  prettify = unlines . map prettify' . M.assocs
    where prettify' (i, t) = show i ++ ". " ++ prettify t

instance ToRecord TodoList where
  toRecord = unlines . map toRecord' . M.assocs
    where toRecord' (i, t) = show i ++ ";" ++ toRecord t

instance FromRecord (Int, Todo) where
  fromRecord s = (,) i <$> t
    where (i, t) = bimap read fromRecord $ breakOn' ';' s

instance FromRecord TodoList where
  fromRecord = Just . M.fromList . mapMaybe fromRecord . lines

----------------------- FUNCTIONS
empty :: TodoList
empty = M.empty

count :: TodoList -> Int
count = M.size

get :: Int -> TodoList -> Maybe Todo
get = M.lookup

add :: String -> TodoList -> TodoList
add n tl = M.insert (count tl) (newTodo n) tl

doTodo :: Int -> TodoList -> TodoList
doTodo = M.adjust setDone

undoTodo :: Int -> TodoList -> TodoList
undoTodo = M.adjust setNotDone

filterTodos :: (Todo -> Bool) -> TodoList -> TodoList
filterTodos = M.filter
