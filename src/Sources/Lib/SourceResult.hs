module Sources.Lib.SourceResult where

import Fallible.Throwing
import Message

newtype SourceError = SourceError Text
  deriving (Show)
