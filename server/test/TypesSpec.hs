module TypesSpec (spec) where

import           Data.Aeson
import           Test.Hspec
import           Test.QuickCheck
import           Types

instance Arbitrary PlayerCommand where
  arbitrary = elements allPlayerCommands

spec :: Spec
spec =
  do describe "Type tests" $
       do it "PlayerCommand <-> JSON is isomorphic" $
            property $
            \cmd -> eitherDecode (encode cmd) == Right (cmd :: PlayerCommand)
