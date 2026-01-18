{-# LANGUAGE UndecidableInstances #-}

module Network.Network where

import Fallible.Throwing
import Network.Class
import Network.HTTP.Req

instance [NetworkError, IOE] :>> effs => MonadHttp (Eff effs) where
  handleHttpException = throw
  getHttpConfig =
    pure
      defaultHttpConfig
        { httpConfigRetryJudgeException = \_ _ -> False
        , httpConfigRetryJudge = \_ _ -> False
        }

runOnInternet :: forall es. _ => Interprets Network es
runOnInternet = interpret \sender ->
  pure . \case
    Get url options -> getReqBody $ sender @IOE $ req GET url NoReqBody bsResponse options
    Post url body options -> getReqBody $ sender @IOE $ req POST url body jsonResponse options
    Put url body options -> getReqBody $ sender @IOE $ req PUT url (ReqBodyJson body) jsonResponse options
 where
  getReqBody :: HttpResponse response => Eff effs response -> Eff effs (HttpResponseBody response)
  getReqBody = fmap responseBody
