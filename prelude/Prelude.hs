{-# LANGUAGE DeriveAnyClass #-}

module Prelude (
  module Prelude,
)
where

import BasePrelude as Prelude

import Control.Applicative as Prelude
import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Monad as Prelude
import Control.Monad.IO.Class as Prelude (liftIO)
import Data.ByteString as Prelude (ByteString)
import Data.Data as Prelude (Typeable)
import Data.Foldable as Prelude
import Data.Foldable.WithIndex as Prelude
import Data.Functor as Prelude hiding (unzip)
import Data.Functor.Foldable as Prelude (Recursive (cata))
import Data.Functor.Foldable.TH as Prelude (MakeBaseFunctor (makeBaseFunctor))
import Data.Hashable (Hashable (hash))
import Data.Hashable as Prelude (Hashable)
import Data.Kind as Prelude (Constraint, Type)
import Data.Maybe as Prelude
import Data.Monoid as Prelude
import Data.Semigroup as Prelude hiding (First, Last, getFirst, getLast)
import Data.Text as Prelude (Text, pack, replace, splitOn, unpack)
import Data.Text.Encoding as Prelude (decodeUtf8, encodeUtf8)
import Data.Traversable.WithIndex as Prelude
import Deriving.Aeson.Stock as Prelude
import GHC.Records as Prelude
import Generic.Data as Prelude (Generically (..))
import Network.HTTP.Req as Prelude (
  HttpBody,
  HttpException,
  Option,
  ReqBodyBs (ReqBodyBs),
  ReqBodyJson (ReqBodyJson),
  Scheme (Https),
  Url,
  header,
  https,
  oAuth2Bearer,
  queryParamToList,
  renderUrl,
  useHttpsURI,
  (/:),
  (/~),
  (=:),
 )
import Numeric.Natural as Prelude
import PyF (fmt)
import Text.Read
import Theseus.Eff as Prelude hiding (Eff, runEff)
import Theseus.Eff qualified as The (Eff)
import Theseus.Effect.IO as Prelude
import Theseus.Effect.Input as Prelude
import Theseus.Effect.Output as Prelude
import Theseus.Effect.State as Prelude (State, modify, runState)

deriving instance Hashable Scheme
deriving instance Hashable (Url Https)
deriving via Generically Scheme instance FromJSON Scheme
deriving via Generically (Url Https) instance FromJSON (Url Https)
deriving via Generically Scheme instance ToJSON Scheme
deriving via Generically (Url Https) instance ToJSON (Url Https)

type Eff = The.Eff Traversable

type f ~> g = forall a. f a -> g a

data MyUrl = HttpsUrl (Url Https) | MxcUrl Text
  deriving (Generic, Eq, Show, Hashable, FromJSON, ToJSON)

getUrl :: MyUrl -> Text
getUrl (HttpsUrl url) = renderUrl url
getUrl (MxcUrl url) = url

f = fmt

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mBool mTrue mFalse = mBool >>= \condition -> if condition then mTrue else mFalse

-- interpretFromHandler :: forall e esSend es otherE. (e :> es, Handling esSend otherE es) => Eff (e : esSend) ~> Eff es
-- interpretFromHandler action = withFromEff \toHandler -> interpret (toHandler . sendVia (toEff . toEff)) action

-- sendHandler :: forall esSend es e otherE. (e :> es, Handling esSend otherE es) => e (Eff esSend) ~> Eff es
-- sendHandler = sendVia \action -> withFromEff @esSend \toHandler -> toHandler $ toEff action

tshow :: Show a => a -> Text
tshow = pack . show

type Interprets e es = Eff (e : es) ~> Eff es

-- reinterpret :: eff :> es => (eff (Eff (eff : es)) ~> Eff es) -> Eff (eff : es) ~> Eff es
-- reinterpret f = interpret_ \eff -> f eff

runOutputMonoid :: Monoid o => Eff (Output o : es) a -> Eff es (o, a)
runOutputMonoid = using (runState mempty) $ interpret_ \(Output o) -> modify (<> o)

readMaybe :: Read a => Text -> Maybe a
readMaybe = Text.Read.readMaybe . unpack

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = ifM condition action $ pure ()

evaluate :: (Traversable t, Applicative f) => t (f a) -> f (t a)
evaluate = sequenceA

hash :: Hashable a => a -> Text
hash = pack . show . Data.Hashable.hash

data Fresh u :: Effect where
  Fresh :: Fresh u m u

fresh :: Fresh u :> es => Eff es u
fresh = send Fresh

runFreshMVar :: IOE :> es => Eff (Fresh Int : es) ~> Eff es
runFreshMVar action = do
  counter <- liftIO $ newMVar 0
  with action $ interpret_ \Fresh -> do
    count <- liftIO $ takeMVar counter
    liftIO $ putMVar counter $ count + 1
    pure count

inputs :: Input i :> es => (i -> a) -> Eff es a
inputs f = fmap f input

runIOE :: Eff '[IOE] ~> IO
runIOE = runEffIO . unrestrict

ignoreOutput :: Eff (Output o : es) ~> Eff es
ignoreOutput = interpret_ \(Output _) -> pure ()
