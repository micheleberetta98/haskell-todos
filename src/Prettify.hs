module Prettify where

class Pretty a where
  prettify :: a -> String
