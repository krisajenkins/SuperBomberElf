{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module EventSystemSpec (spec) where

import qualified Data.Map                  as Map
import           Data.UUID
import           EventSystem
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Utils

instance Arbitrary UUID where
  arbitrary = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do scheduleSpec

scheduleSpec :: Spec
scheduleSpec =
  describe "Schedule Handling" $
  it "Future events are not processed." . property $
  \(t,(e1 :: String,e2 :: String)) ->
    let eventsFrom = fst . dueEvents t . scheduleFrom Map.empty
    in shouldBe (eventsFrom [(e1,t)])
                (eventsFrom [(e1,t),(e2,addTime 5 t)])
