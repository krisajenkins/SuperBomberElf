module Spec where

import           Data.Aeson
import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.Types

main :: IO ()
main = defaultMain [Test.Types.tests]
