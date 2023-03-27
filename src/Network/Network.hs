module Network.Network where

import Network.HTTP.Req
import Network.Class

runOnInternet :: forall es. IOE :> es => Eff (Network : es) ~> Eff es
runOnInternet = interpret handler
  where
    handler :: forall esSend. (Handling esSend Network es) => Network (Eff esSend) ~> Eff es
    handler = withOuterIO . \case
      Get url options -> getReqBody $ req GET url NoReqBody bsResponse options
      Post url body options -> getReqBody $ req POST url body jsonResponse options
      Put url body options -> getReqBody $ req PUT url (ReqBodyJson body) jsonResponse options
      where
        getReqBody :: HttpResponse response => Req response -> IO (HttpResponseBody response)
        getReqBody = fmap responseBody . runReq defaultHttpConfig

        withOuterIO :: IO a -> Eff es a
        withOuterIO action = toEff $ fromIO @esSend action