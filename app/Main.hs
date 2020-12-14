module Main where

import           Control.Monad.State (runState)
import           System.Environment
import qualified TodoList            as TL
import           TodoState           (ListAction (..), TodoState,
                                      UpdateAction (..), list, update)

import           Record              (readFromFile, writeToFile)

data Command = Add String
  | Do Int
  | Undo Int
  | Invalid String
  | ListAll
  | ListDone
  | ListNotDone
  | Quit

main :: IO ()
main = do
  -- args <- getArgs
  putStrLn "==================================="
  putStrLn "Command:"
  putStrLn "  + [content]    Add new todo"
  putStrLn "  d [i]          Set todo done"
  putStrLn "  u [i]          Set todo undone"
  putStrLn "  ls             List all todos"
  putStrLn "  ld             List done todos"
  putStrLn "  ln             List not done todos"
  putStrLn "  q              Quit"
  putStrLn "==================================="

  -- let filename = head args
  let filename = "test.txt"
  ts <- readFromFile filename

  loop filename ts

loop :: FilePath -> TL.TodoList -> IO ()
loop filename s = do
  putStr " > "
  line <- getLine
  let cmd = parse line
  result <- execute cmd s filename
  mapM_ (loop filename) result

parse :: String -> Command
parse ('+':' ':name) = Add name
parse ('d':' ':int)  = Do (read int)
parse ('u':' ':int)  = Undo (read int)
parse ('l':'s':_)    = ListAll
parse ('l':'d':_)    = ListDone
parse ('l':'n':_)    = ListNotDone
parse ('q':_)        = Quit
parse s              = Invalid s

execute :: Command -> TL.TodoList -> FilePath -> IO (Maybe TL.TodoList)
execute (Invalid line) s _ = do
  putStrLn $ "Invalid command: `" ++ line ++ "`"
  return (Just s)
execute Quit s filename = do
  putStrLn "Bye!"
  writeToFile filename s
  return Nothing
execute cmd s _ = do
  let (tl, s') = runState (apply cmd) s
  printResult tl
  return (Just s')

printResult :: TL.TodoList -> IO ()
printResult tl
  | TL.count tl == 0 = return ()
  | otherwise        = print tl

apply :: Command -> TodoState TL.TodoList
apply (Add name)  = update (AddTodo name) >> return TL.empty
apply (Do i)      = update (SetTodoDone i) >> return TL.empty
apply (Undo i)    = update (UnsetTodoDone i) >> return TL.empty
apply ListAll     = list ListAllTodos
apply ListDone    = list ListDoneTodos
apply ListNotDone = list ListNotDoneTodos
apply Quit        = return TL.empty
