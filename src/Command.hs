module Command (Command(..), CommandResult(..), parse, execute) where

import           Control.Monad.State (runState)
import           TodoList            (TodoList, empty)
import           TodoState           (ListAction (..), TodoState,
                                      UpdateAction (..), list, update)

----------------------- DATA
data Command =
  Add String
  | Do Int
  | Undo Int
  | ListAll
  | ListDone
  | ListNotDone
  | Quit
  | Invalid String
  deriving (Show, Eq)

data CommandResult = OkContinue (TodoList, TodoList)
  | OkQuit String
  | NotOk String
  deriving (Show, Eq)

----------------------- FUNCTIONS
parse :: String -> Command
parse = uncurry parse' . split
  where
    parse' "+"    (Just name) = Add name
    parse' "do"   (Just int)  = Do (read int)
    parse' "undo" (Just int)  = Undo (read int)
    parse' "ls"   _           = ListAll
    parse' "ld"   _           = ListDone
    parse' "ln"   _           = ListNotDone
    parse' "q"    _           = Quit
    parse' cmd    _           = Invalid cmd

split :: String -> (String, Maybe String)
split s = (cmd, arg)
  where
    (cmd, rest) = break (==' ') s
    arg = tail' rest

tail' :: String -> Maybe String
tail' "" = Nothing
tail' s  = Just $ tail s

execute :: Command -> TodoList -> CommandResult
execute Quit        _ = OkQuit "Bye!"
execute (Invalid s) _ = NotOk $ "Invalid command `" ++ s ++ "`"
execute cmd         s = OkContinue $ runState (apply cmd) s

apply :: Command -> TodoState TodoList
apply (Add name)  = update (AddTodo name) >> return empty
apply (Do i)      = update (SetTodoDone i) >> return empty
apply (Undo i)    = update (UnsetTodoDone i) >> return empty
apply ListAll     = list ListAllTodos
apply ListDone    = list ListDoneTodos
apply ListNotDone = list ListNotDoneTodos
apply Quit        = return empty
