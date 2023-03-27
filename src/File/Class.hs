{-# LANGUAGE TemplateHaskell #-}

module File.Class where

data File :: Effect where
    GetFile :: Text -> File m ByteString
makeEffect ''File
