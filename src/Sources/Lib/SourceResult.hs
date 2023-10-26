module Sources.Lib.SourceResult where

newtype SourceError = SourceError Text
  deriving (Show)
