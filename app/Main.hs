module Main where

import           Command   (CommandResult (..), execute, parse)
import           Record    (readFromFile, writeToFile)
import           System.IO (hFlush, stdout)
import           TodoList  (TodoList, count)

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
  ts' <- loop ts
  writeToFile filename ts'

loop :: TodoList -> IO TodoList
loop s = do
  line <- prompt "> "
  let
    cmd = parse line
    result = execute cmd s
  printResult result
  case result of
    OkContinue (_, s') -> loop s'
    OkQuit     _       -> return s
    NotOk      _       -> loop s

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

printResult :: CommandResult -> IO ()
printResult (OkQuit msg) = putStrLn msg
printResult (NotOk msg) = putStrLn msg
printResult (OkContinue (tl, _))
  | count tl == 0 = return ()
  | otherwise     = print tl
