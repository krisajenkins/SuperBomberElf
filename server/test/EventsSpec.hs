{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module EventsSpec (spec) where

import           Control.Arrow
import qualified Data.Map                  as Map
import           Data.UUID
import           Events
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Utils

instance Arbitrary UUID where
  arbitrary = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Event where
  arbitrary =
    oneof [AddPlayer <$> arbitrary
          ,SetPlayerName <$> arbitrary <*> arbitrary
          ,RemovePlayer <$> arbitrary]

spec :: Spec
spec = do scheduleSpec
          eventSpec

eventsCause
  :: [(Event,Time)] -> Time -> (Scene,[Reaction]) -> Expectation
eventsCause events time expected =
  sceneAt schedule time `shouldBe` expected
  where schedule = scheduleAt Map.empty events

scheduleSpec :: Spec
scheduleSpec =
  describe "Schedule Handling" $
  it "Future events are not processed." . property $
  \(t,(e1,e2)) ->
    (sceneAt (scheduleAt Map.empty
                         [(e1,t)])
             t) `shouldBe`
    (sceneAt (scheduleAt Map.empty
                         [(e1,t),(e2,addTime 5 t)])
             t)

eventSpec :: Spec
eventSpec =
  describe "Event Processing" $
  do it "Join and name should result in one named player" . property $
       \(t,uuid,name) ->
         eventsCause [(AddPlayer uuid,t),(SetPlayerName uuid name,addTime 5 t)]
                     (addTime 10 t)
                     (Scene {_players = Map.fromList [(uuid,Player name)]},[])
     it "Removing a non-existent player shouldn't matter." . property $
       \(t,uuid) ->
         eventsCause [(RemovePlayer uuid,t)]
                     (addTime 10 t)
                     (Scene Map.empty,[])
     it "Adding n players results in n players." . property $
       \((NonEmpty srcs) :: NonEmptyList (UUID,Time)) ->
         let events = fmap (first AddPlayer) srcs
             time = maximum (snd <$> srcs)
             (scene,_) = sceneAt (scheduleAt Map.empty events) time
         in length (_players scene) == length srcs
     it "Double adding a player causes an error." . property $
       \(t1,t2,uuid) ->
         eventsCause
           [(AddPlayer uuid,t1),(AddPlayer uuid,t2)]
           (max t1 t2)
           (Scene {_players = Map.fromList [(uuid,Player Nothing)]}
           ,[PlayerAlreadyAdded uuid])
