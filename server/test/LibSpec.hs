{-# OPTIONS_GHC -fno-warn-orphans #-}
module LibSpec (spec) where

import qualified Data.Map                  as Map
import           Lib
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           TypesSpec                 ()

instance Arbitrary Server where
  arbitrary =
    do scene <- arbitrary
       return Server {_clients = Map.empty
                     ,_scene = scene}

spec :: Spec
spec = pure ()

-- connectionManagementSpec :: Spec
-- connectionManagementSpec =
--   describe "Connection Management" $
--   do it "Generating a UUID ticks StdGen" . property $
--        \(Blind server) ->
--          let (uuid1,server') = runState genUUID server
--              (uuid2,_) = runState genUUID server'
--          in (uuid1 /= uuid2)
