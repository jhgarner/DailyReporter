{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sources.Lib (module Sources.Lib) where

import File.Class as Sources.Lib
import Network.Class as Sources.Lib
import Parser.HtmlParser as Sources.Lib
import Parser.JsonParser as Sources.Lib
import Config as Sources.Lib
import Data.Time as Sources.Lib (ZonedTime)
import Data.Time.Clock.POSIX as Sources.Lib (posixSecondsToUTCTime)
import Data.Time.Format as Sources.Lib (defaultTimeLocale, formatTime)
import Data.Time.LocalTime as Sources.Lib
  ( TimeZone,
    hoursToTimeZone,
    utcToZonedTime,
  )
import qualified Cleff.State as State
import Control.Arrow
import Fallible.Retryable
import Fallible.Throwing (runThrowing, Throw, throw, rethrow)

type Params = Map Text Text
type SourceResult = Either Text Params
type Source es = (Text, Eff es SourceResult)
type EvaluatedSource = (Text, SourceResult)

nameOf :: Source es -> Text
nameOf (name, _) = name

data SourceFactory :: Effect where
    UsingHtmlUrl :: Url 'Https -> SourceFactory m ()
    UsingHtml :: ByteString -> SourceFactory m ()
    UsingJsonUrl :: Url 'Https -> Option 'Https -> SourceFactory m ()
    IsEqualTo :: Text -> Text -> SourceFactory m ()
    AdjustedBy :: Text -> (Text -> Text) -> SourceFactory m ()
makeEffect ''SourceFactory

withCustomParserError :: _ => Text -> Eff es a -> Eff es a
withCustomParserError errorText = (>>) $ State.put @(Maybe Text) (Just errorText)

type SourceFactoryEnv es = Eff (SourceFactory:NetworkError:State (Maybe Text):State Params:Throw Text:es)

runSourceFactory :: ([File, Network, Retryable HttpException] :>> es) => Text -> SourceFactoryEnv es a -> Eff es SourceResult
runSourceFactory name = runThrowing . execState mempty . execState mempty . handleErrors . interpret \case
  UsingHtmlUrl url -> do
    site <- get url mempty
    guide <- getFile [f|parsers/{name}.json|]
    add $ extractHtml guide site

  UsingHtml site -> do
    guide <- getFile [f|parsers/{name}.json|]
    add $ extractHtml guide site

  UsingJsonUrl url options -> do
    site <- get url options
    guide <- getFile [f|parsers/{name}.json|]
    errorText <- State.gets @(Maybe Text) $ fromMaybe [f|Failed to parse {show url}|]
    maybe (throw errorText) add $ extractJson guide site

  IsEqualTo key value ->
    add $ singleton [f|*{key}|] value

  AdjustedBy key action ->
    modify @Params $ update (Just . action) [f|*{key}|]

  where
    add :: _ => Params -> Eff es ()
    add = modify . mappend
    handleErrors = handleFailureWith $ throw . pack . show

makeSource :: _ => Text -> SourceFactoryEnv es a -> Source es
makeSource name action = (name, runSourceFactory name action)
