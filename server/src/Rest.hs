{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Rest (application) where

import           Network.Wai as Wai
import           Servant

type API = "view" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "client/dist"

application :: Application
application = serve api server
