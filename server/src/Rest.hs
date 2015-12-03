{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Rest (application) where

import           Network.Wai as Wai
import           Servant

type API = "view" :> Raw

api :: Proxy API
api = Proxy

application :: FilePath -> Application
application = serve api . serveDirectory
