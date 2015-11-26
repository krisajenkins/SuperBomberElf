module TypesSpec (spec) where

import           Data.Aeson
import           Data.Maybe
import           Data.Time
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Types

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary <*> arbitrary

instance Arbitrary Player where
  arbitrary = Player <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PlayerCommand where
  arbitrary = elements allPlayerCommands

spec :: Spec
spec = do playerCommandSpec
          bombSpec
          respawnSpec

respawnSpec :: Spec
respawnSpec =
  describe "Respawn" $
  do it "Living things don't change." $
       property $ \now delay -> isNothing $ respawn now delay Nothing
     it "Dead things will respawn." $
       property $
       \now delay t ->
         if addUTCTime delay t < now
            then isNothing $ respawn now delay (Just t)
            else respawn now delay (Just t) == Just t

bombSpec :: Spec
bombSpec =
  describe "Player" $
       do it "Players without a death time should always be alive" $
            property $
            \player time -> not $ isDead time player {_playerDiedAt = Nothing}
          it "Players who die in the future should always be alive" $
            property $
            \player time (Positive seconds) ->
              not $
              isDead time
                     player {_playerDiedAt = Just $ addUTCTime seconds time}
          it "Players who die in the past should always be dead" $
            property $
            \player time (Positive seconds) ->
              isDead (addUTCTime seconds time)
                     player {_playerDiedAt = Just time}

playerCommandSpec :: Spec
playerCommandSpec =
  describe "PlayerCommand" $
       it "PlayerCommand <-> JSON is isomorphic" $
       property $
       \cmd -> eitherDecode (encode cmd) == Right (cmd :: PlayerCommand)
