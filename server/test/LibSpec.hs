{-# OPTIONS_GHC -fno-warn-orphans #-}
module LibSpec (spec) where

import           Control.Monad.State
import qualified Data.Map                  as Map
import           Lib
import           System.Random
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           TypesSpec                 ()

instance Arbitrary Server where
  arbitrary =
    do seed <- arbitrary
       scene <- arbitrary
       pure Server {_clients = Map.empty
                   ,_scene = scene
                   ,_generator = mkStdGen seed}

spec :: Spec
spec = pure ()
