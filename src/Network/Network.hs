{-# LANGUAGE UndecidableInstances #-}

module Network.Network where

import Network.HTTP.Req
import Network.Class
import Fallible.Throwing

instance [NetworkError, IOE] :>> effs => MonadHttp (Eff effs) where
  handleHttpException = throw

runOnInternet :: forall es. IOE :> es => Eff (Network : es) ~> Eff es
runOnInternet = interpret handler
  where
    handler :: forall esSend. (Handling esSend Network es) => Network (Eff esSend) ~> Eff es
    handler = interpretFromHandler @IOE @esSend . \case
      Get url options -> getReqBody $ req GET url NoReqBody bsResponse options
      Post url body options -> getReqBody $ req POST url body jsonResponse options
      Put url body options -> getReqBody $ req PUT url (ReqBodyJson body) jsonResponse options
      where
        getReqBody :: HttpResponse response => Eff effs response -> Eff effs (HttpResponseBody response)
        getReqBody = fmap responseBody