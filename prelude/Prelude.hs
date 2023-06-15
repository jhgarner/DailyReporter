module Prelude
  ( module Prelude,
  )
where

import Text.Read
import BasePrelude as Prelude
import Cleff as Prelude
import Cleff.Input as Prelude
import Cleff.Output as Prelude
import Cleff.Trace as Prelude
import Cleff.Fresh as Prelude
import Cleff.State as Prelude (modify, runState, State)
import Control.Applicative as Prelude
import Control.Monad as Prelude
import Data.ByteString as Prelude (ByteString)
import Data.Data as Prelude (Typeable)
import Data.Foldable as Prelude
import Data.Foldable.WithIndex as Prelude
import Data.Functor as Prelude
import Data.Functor.Foldable as Prelude (Recursive (cata))
import Data.Functor.Foldable.TH as Prelude (MakeBaseFunctor (makeBaseFunctor))
import Data.Hashable (Hashable (hash))
import Data.Hashable as Prelude (Hashable)
import Data.Kind as Prelude (Type)
import Data.Map.Strict as Prelude (Map, fromList, insert, singleton, update)
import Data.Maybe as Prelude
import Data.Monoid as Prelude
import Data.Semigroup as Prelude hiding (First, Last, getFirst, getLast)
import Data.Text as Prelude (Text, pack, replace, splitOn, unpack)
import Data.Text.Encoding as Prelude (decodeUtf8, encodeUtf8)
import Data.Traversable.WithIndex as Prelude
import Deriving.Aeson.Stock as Prelude
import GHC.Records as Prelude
import Network.HTTP.Req as Prelude
  ( HttpBody,
    HttpException,
    Option,
    ReqBodyBs (ReqBodyBs),
    ReqBodyJson (ReqBodyJson),
    Scheme (Https),
    Url,
    header,
    https,
    queryParamToList,
    oAuth2Bearer,
    useHttpsURI,
    (/:),
    (/~),
    (=:),
  )
import Numeric.Natural as Prelude
import PyF (fmt)

f = fmt

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mBool mTrue mFalse = mBool >>= \condition -> if condition then mTrue else mFalse

interpretFromHandler :: forall e esSend es otherE. (e :> es, Handling esSend otherE es) => Eff (e : esSend) ~> Eff es
interpretFromHandler action = withFromEff \toHandler -> interpret (toHandler . sendVia (toEff . toEff)) action

sendHandler :: forall esSend es e otherE. (e :> es, Handling esSend otherE es) => e (Eff esSend) ~> Eff es
sendHandler = sendVia \action -> withFromEff @esSend \toHandler -> toHandler $ toEff action

tshow :: Show a => a -> Text
tshow = pack . show

type Interprets e es = Eff (e : es) ~> Eff es

runOutputMonoid :: Monoid o => Eff (Output o:es) a -> Eff es (a, o)
runOutputMonoid = runState mempty . reinterpret \(Output o) -> modify (<> o)

readMaybe :: Read a => Text -> Maybe a
readMaybe = Text.Read.readMaybe . unpack

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = ifM condition action $ pure ()

execState :: s -> Eff (State s:es) a -> Eff es s
execState s = fmap snd . runState s

evaluate :: (Traversable t, Applicative f) => t (f a) -> f (t a)
evaluate = sequenceA

hash :: Hashable a => a -> Text
hash = pack . show . Data.Hashable.hash
