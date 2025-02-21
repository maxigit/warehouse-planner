{-# Language BlockArguments #-}
module Planner.PartitionModeSpec where

import ClassyPrelude
import Test.Hspec
import Planner.SpecUtil
import WarehousePlanner.Base
import WarehousePlanner.Move
import WarehousePlanner.Exec


spec :: Spec
spec = parallel pureSpec

pureSpec :: Spec
pureSpec = do
  sortedSpec
  
getBoxes = map fst . includedList

sortedSpec :: Spec
sortedSpec = describe "POverlapSorted" do
   context "One shelf" do
           it "insert in between" do
              let action = ( [ "S A-1 A-2"
                             , "S A-5"
                             ] `addSorted` ["S A-3"]
                             )
              action `shouldWH` ["A-3 S '1:1:3"]
           it "insert after" do
              let action = ( [ "S A-1 A-2"
                             , "S A-5"
                             ] `addSorted` ["S A-6"]
                             )
              action `shouldWH` ["A-6 S '2:1:1"]
   context "ExitOnTop" do
           it "uses T for the fith box" do -- test the test
              let action = [] `addSorted` ["S|T A-1 A-2 A-3 A-4 A-5 A-6 A-7"]
              action `shouldWH` [ "A-1 S '1:1:1"
                                , "A-2 S '1:1:2"
                                , "A-3 S '1:1:3"
                                , "A-4 T '1:1:1" -- use T 
                                , "A-5 T '1:1:2"
                                , "A-6 T '1:1:3"
                                , "A-7 S '2:1:1" -- use S again
                                ]
           it "insert in between" do
              let action = ( [ "S A-1 A-2", "T A-5"
                             ] `addSorted` ["S|T A-3"]
                             )
              action `shouldWH` ["A-3 S '1:1:3"]
           it "insert after" do
              let action = ( [ "S A-1 A-2" , "T A-5"
                             ] `addSorted` ["S|T A-6"]
                             )
              action `shouldWH` ["A-6 T '1:1:2"]


        
-- * For simple shelf
exec0 = execWH (emptyWarehouse $ fromGregorian 2025 02 21) . (withBoxOrientations ors )
    where ors = parseOrientationRule [] "!'"

addSorted :: [Text] -> [Text] -> WH [Text] s
addSorted boxes newBoxes =  do
        makeShelves $ words "S T pending"
        boxes <- makeBoxesWithPartition PRightOnly boxes
        newBoxes <- map fst <$> makeBoxesWithPartition PSortedOverlap newBoxes
        mapM (\(box,i) -> expandAttribute box i "${boxname} ${shelfname} ${position-spec}" )
             (zip newBoxes [1..])
        showBoxWithPosition newBoxes
             
showBoxWithPosition :: [Box s] -> WH [Text] s
showBoxWithPosition boxes = mapM (\(box,i) -> expandAttribute box i "${boxname} ${shelfname} ${position-spec}" ) 
                                 (zip boxes [1..])


shouldWH :: Eq a => Show a => WH [a] RealWorld  -> [a] -> IO ()
shouldWH action expected =  exec0 action `shouldReturn` expected
     
      
