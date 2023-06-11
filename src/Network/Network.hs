{-# LANGUAGE UndecidableInstances #-}

module Network.Network where

import Network.HTTP.Req
import Network.Class
import Fallible.Throwing

instance [NetworkError, IOE] :>> effs => MonadHttp (Eff effs) where
  handleHttpException = throw
  getHttpConfig = pure defaultHttpConfig
    { httpConfigRetryJudgeException = \_ _ -> False
    , httpConfigRetryJudge = \_ _ -> False
    }

runOnInternet :: forall es. _ => Interprets Network es
runOnInternet = interpret handler
  where
    -- What's up with esSend? When processing an effect, there are 2 contexts:
    -- 1) The context of the Handler which has access to all effects that
    -- haven't been interpreted yet. These effects are implementation details
    -- and are hidden from the users of Network.
    -- 2) The context of the sender which has access to all effects at the point
    -- of calling get, post, or put. These effects are part of Network's Api and
    -- the user of Network must prove they exist.
    --
    -- The variable es is the first one. esSend is the second. We use
    -- NetworkError from the esSend context and IOE from the es context.
    --
    -- Why do we have to use NetworkError from the esSend context? Users should
    -- be able to handle NetworkErrors however they choose. That could mean
    -- deferring to their caller, or using some retry mechanism. Users can only
    -- handle NetworkError effects if it's part of the Api, so we must use
    -- esSend.
    --
    -- Why do we have to use IOE from es? Whether the Network effect uses IO or
    -- not is an implementation detail that users don't care about. We lose
    -- testability if we make IOE part of the Api of Network.
    handler :: forall esSend. (Handling esSend Network es) => Network (Eff esSend) ~> Eff es
    handler = interpretFromHandler @IOE @esSend . \case
      Get url options -> getReqBody $ req GET url NoReqBody bsResponse options
      Post url body options -> getReqBody $ req POST url body jsonResponse options
      Put url body options -> getReqBody $ req PUT url (ReqBodyJson body) jsonResponse options
      where
        getReqBody :: HttpResponse response => Eff effs response -> Eff effs (HttpResponseBody response)
        getReqBody = fmap responseBody
