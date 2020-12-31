module StringUtils
  ( breakOn
  , breakOn'
  ) where

import           Data.Bifunctor (Bifunctor (second))
import           Data.Maybe     (fromMaybe)

breakOn :: Char -> String -> (String, Maybe String)
breakOn c = second safeTail . break (==c)

breakOn' :: Char -> String -> (String, String)
breakOn' c = second (fromMaybe "") . breakOn c

safeTail :: String -> Maybe String
safeTail "" = Nothing
safeTail s  = Just (tail s)
