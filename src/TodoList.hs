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
import           Prettify       (Pretty (..))
import           Record         (FromRecord (..), ToRecord (..))
import           Todo           (Todo, newTodo, setDone, setNotDone)

----------------------- DATA
type TodoList = M.Map Int Todo

----------------------- INSTANCES
instance Pretty TodoList where
  prettify = unlines . map prettify' . M.assocs
    where prettify' (i, t) = show i ++ ". " ++ prettify t

instance ToRecord TodoList where
  toRecord = unlines . map toRecord' . M.assocs
    where
      toRecord' (i, t) = show i ++ ";" ++ toRecord t

instance FromRecord TodoList where
  fromRecord = M.fromList . map fromRecord' . filter (/="") . lines
    where
      fromRecord' = second fromRecord . parts

      parts :: String -> (Int, String)
      parts s = bimap read tail $ break (==';') s

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
