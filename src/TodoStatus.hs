module TodoStatus  (TodoStatus(..)) where

import           Prettify (Pretty (..))

data TodoStatus = Done | NotDone deriving (Show, Eq)

instance Pretty TodoStatus where
  prettify Done    = "✅"
  prettify NotDone = "❌"
