{-# LANGUAGE TemplateHaskell #-}

module File.Filesystem where

import Data.FileEmbed
import File.Class
import Data.Map (mapKeys, (!), keys)

runWithFilesystem :: Interprets File es
runWithFilesystem = interpret \case
    GetFile name -> pure $ filesystem ! unpack name

filesystem :: Map FilePath ByteString
filesystem = fromList $(makeRelativeToProject "data" >>= embedDir)
