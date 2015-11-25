module Test.Types (tests) where

import           Data.Aeson
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Types

instance Arbitrary PlayerCommand where
  arbitrary = elements allPlayerCommands

commandJsonIsIsomorphic :: PlayerCommand -> Bool
commandJsonIsIsomorphic cmd = Right cmd == eitherDecode (encode cmd)

tests =
  testGroup "TypeTests" [testProperty "Commands <-> JSON works" commandJsonIsIsomorphic]
