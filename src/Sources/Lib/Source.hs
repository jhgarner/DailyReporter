module Sources.Lib.Source where

import Fallible.Throwing
import Message
import Sources.Lib.SourceFactory
import Sources.Lib.SourceResult

data Source es = Source {name :: Text, sourceAction :: Eff (Throw SourceError : es) [Message]}

makeSource :: _ => Text -> SourceFactoryEnv es a -> Source es
makeSource name action = Source name $ runSourceFactory name action
