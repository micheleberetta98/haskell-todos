module Record
  ( ToRecord
  , FromRecord
  , toRecord
  , fromRecord
  , readFromFile
  , writeToFile
  ) where

import           Data.Functor ((<&>))

class ToRecord a where
  toRecord :: a -> String

class FromRecord a where
  fromRecord :: String -> Maybe a

readFromFile :: (FromRecord a) => FilePath -> IO (Maybe a)
readFromFile filename = readFile filename <&> fromRecord

writeToFile :: (ToRecord a) => FilePath -> a -> IO ()
writeToFile filename = writeFile filename . toRecord
