module Record
  ( ToRecord
  , FromRecord
  , toRecord
  , fromRecord
  , readFromFile
  , writeToFile
  ) where

class ToRecord a where
  toRecord :: a -> String

class FromRecord a where
  fromRecord :: String -> Maybe a

readFromFile :: (FromRecord a) => FilePath -> IO (Maybe a)
readFromFile filename = fromRecord <$> readFile filename

writeToFile :: (ToRecord a) => FilePath -> a -> IO ()
writeToFile filename = writeFile filename . toRecord
