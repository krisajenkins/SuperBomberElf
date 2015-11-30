{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Rest (application) where

import           Network.Wai as Wai
import           Servant

type API = "code" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "static"

application :: Application
application = serve api server
