module Main where

import           Command            (CommandResult (..), execute, parse)
import           Control.Monad      (unless)
import           Data.Maybe         (fromMaybe)
import           Prettify           (prettify)
import           Record             (readFromFile, writeToFile)
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)
import           System.IO          (hFlush, stdout)
import           TodoList           (TodoList, count, empty)

main :: IO ()
main = do
  fname <- getArgs >>= validateArgs
  unless (null fname) $ do
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
    readFromFile fname >>= loop . fromMaybe empty >>= writeToFile fname

validateArgs :: [String] -> IO FilePath
validateArgs []        = putStrLn "Missing filename (first argument)" >> return ""
validateArgs (fname:_) = do
  ok <- doesFileExist fname
  if ok
    then return fname
    else putStrLn ("No such file: `" ++ fname ++ "`") >> return ""

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
printResult (NotOk msg)  = putStrLn msg
printResult (OkContinue (tl, _))
  | count tl == 0        = return ()
  | otherwise            = putStrLn (prettify tl)
