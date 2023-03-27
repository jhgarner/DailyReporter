module Prelude
  ( module Prelude,
  )
where

import BasePrelude as Prelude
import Cleff as Prelude
import Cleff.Input as Prelude
import Cleff.Output as Prelude
import Cleff.Trace as Prelude
import Control.Applicative as Prelude
import Control.Monad as Prelude
import Data.ByteString as Prelude (ByteString)
import Data.Data as Prelude (Typeable)
import Data.Foldable as Prelude
import Data.Foldable.WithIndex as Prelude
import Data.Functor.Foldable as Prelude (Recursive (cata))
import Data.Functor.Foldable.TH as Prelude (MakeBaseFunctor (makeBaseFunctor))
import Data.Hashable as Prelude (Hashable (hash))
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
    oAuth2Bearer,
    useHttpsURI,
    (/:),
    (=:),
  )
import Numeric.Natural as Prelude
import PyF (fmt)

f = fmt

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mBool mTrue mFalse = mBool >>= \condition -> if condition then mTrue else mFalse

interpretFromHandler :: forall e esSend es otherE. (e :> es, Handling esSend otherE es) => Eff (e : esSend) ~> Eff es
interpretFromHandler action = withFromEff \toHandler -> interpret (toHandler . sendVia (toEff . toEff)) action

sendHandler :: forall esSend e es otherE. (e :> es, Handling esSend otherE es) => e (Eff esSend) ~> Eff es
sendHandler = sendVia \action -> withFromEff @esSend \toHandler -> toHandler $ toEff action
