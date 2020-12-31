module Main where

import           Command            (CommandResult (..), execute, parse)
import           Data.Maybe         (fromMaybe)
import           Prettify           (prettify)
import           Record             (readFromFile, writeToFile)
import           System.Environment (getArgs)
import           System.IO          (hFlush, stdout)
import           TodoList           (TodoList, count, empty)

main :: IO ()
main = do
  args <- getArgs
  putStrLn "==================================="
  putStrLn "Command:"
  putStrLn "  + [content]    Add new todo"
  putStrLn "  do [i]         Set todo done"
  putStrLn "  undo [i]       Set todo undone"
  putStrLn "  ls             List all todos"
  putStrLn "  ld             List done todos"
  putStrLn "  ln             List not done todos"
  putStrLn "  q              Quit"
  putStrLn "==================================="

  let filename = head args
  readFromFile filename >>= loop . fromMaybe empty >>= writeToFile filename

loop :: TodoList -> IO TodoList
loop s = do
  line <- prompt "> "
  let result = execute (parse line) s
  printResult result
  case result of
    OkContinue (_, s') -> loop s'
    OkQuit     _       -> return s
    NotOk      _       -> loop s

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

printResult :: CommandResult -> IO ()
printResult (OkQuit msg) = putStrLn msg
printResult (NotOk msg) = putStrLn msg
printResult (OkContinue (tl, _))
  | count tl == 0 = return ()
  | otherwise     = putStrLn (prettify tl)
