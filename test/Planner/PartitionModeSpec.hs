{-# Language BlockArguments #-}
module Planner.PartitionModeSpec where

import ClassyPrelude
import Test.Hspec
import Planner.SpecUtil
import WarehousePlanner.Base
import WarehousePlanner.Move
import WarehousePlanner.Exec
import WarehousePlanner.Slices
import Data.List.NonEmpty (NonEmpty(..))
import Data.Char (isDigit)


spec :: Spec
spec = parallel pureSpec

pureSpec :: Spec
pureSpec = do
  sortedSpec
  utilSpec
  
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
              action `shouldWH` ["A-6 S '2:1:2"]
           it "inserts before and afwer" do
              -- we want to insert in X (A2)
              -- X  X
              -- A1 A2
              -- A1 A2 A3
              let action = [ "S A-1 A-1"
                           , "S A-2 A-2"
                           , "S A-3"
                           ] `addSorted` [ "S A-2 A-2 A-2" ]
              action `shouldWH` [ "A-2 S '1:1:3"
                                , "A-2 S '2:1:3"
                                ]
           it "ignores other styles" do
              -- we want to insert in X (A2)
              --    X    
              -- B4 A1   
              -- B4 A1 A2
              let action = [ "S B-4 B-4"
                           , "S A-1 A-1"
                           , "S A-2 A-2"
                           ] `addSorted` [ "S A-2" ]
              action `shouldWH` [ "A-2 S '2:1:3"
                                ]

   context "priorities" do
        it "given order but keep content sorted" do
              -- we get A-3 and A-2 but in content order
              let action = [ "S A-1" -- A-2 A-3
                           , "S A-4"
                           ] `addSorted` ["S A-3 A-2 A-2"]
              action `shouldWH` [ "A-2 S '1:1:2"
                                , "A-3 S '1:1:3"
                                ]
        it "given order A-2 " do
              let action = [ "S A-1" -- A-2 A-3
                           , "S A-4"
                           ] `addSorted` ["S A-2 A-2 A-3"]
              action `shouldWH` [ "A-2 S '1:1:2"
                                , "A-2 S '1:1:3"
                                ]
        it "content first" do
              let action = [ "S A-1" -- A-2 A-3
                           , "S A-4"
                           ] `addSorted` ["S A-2 A-3 A-3"]
              action `shouldWH` [ "A-2 S '1:1:2"
                                , "A-3 S '1:1:3"
                                ]
        it "given order A-3" do
              -- we want all A-3s instead A-2
              let action = [ "S A-1" -- A-3 A-3
                           , "S A-4"
                           ] `addSorted` ["S A-3 A-3 A-2"]
              action `shouldWH` [ "A-3 S '1:1:2"
                                , "A-3 S '1:1:3"
                                ]
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
   context "use priorities" do
        it "default priority" do
              -- we want 2 A-3 instead of 2 A-2, for this we set a higher priority to A-3 content
              let action = [ "S A-1" -- A-2 A-2 | T A-3 A-3 A-3-#
                           , "S A-4"
                           ] `addSorted` ["S|T A-2 A-2 A-3 A-3 A-3 A-3"]
              action `shouldWH` [ "A-2 S '1:1:2"
                                , "A-2 S '1:1:3"
                                , "A-3 T '1:1:1"
                                , "A-3 T '1:1:2"
                                , "A-3 T '1:1:3"
                                ]
        it "uses content priority across different content " do
              -- we want 2 A-3 instead of 2 A-2, for this we set a higher priority to A-3 content
              let action = [ "S A-1" -- A-2 A-3 | T A-3
                           , "S A-4"
                           ] `addSorted` ["S|T A-2 A-3 A-3 A-3 A-3 A-2"]
              action `shouldWH` [ "A-2 S '1:1:2"
                                , "A-3 S '1:1:3"
                                , "A-3 T '1:1:1"
                                , "A-3 T '1:1:2"
                                , "A-3 T '1:1:3"
                                ]
        it "uses content priority across different content " do
              -- we want 2 A-3 instead of 2 A-2, for this we set a higher priority to A-3 content
              let action = [ "S A-1" -- A-2 A-3 | T A-3
                           , "S A-4"
                           ] `addSorted` ["S|T A-2 A-3 A-2 A-3 A-3 A-3 A-3"]
              action `shouldWH` [ "A-2 S '1:1:2"
                                , "A-2 S '1:1:3"
                                , "A-3 T '1:1:1"
                                , "A-3 T '1:1:2"
                                , "A-3 T '1:1:3"
                                ]

utilSpec = describe "utils" do
     context "addSlotBounds" do
        -- | create slots (number) or overlapping boxes (char)
        let makeSlices = map ((),) . map (\s -> case s of 
                                     [c] | isDigit c -> Right c
                                     "" -> Right '0'
                                     c:cs -> Left ( c :| cs)
                             ) . words
        let pretty = unwords . map \(bound, (_, x)) -> "<"
                                            <> fromMaybe "" (lowerB bound)
                                            <> ":"
                                            <> fromMaybe "" (upperB bound)
                                            <> ">"
                                            <> pack (either toList pure x)
            bound = pretty . addSlotBounds (Just . singleton) .  makeSlices
        it "add nothing" do
           bound "1 2 3 4 5"  `shouldBe` "<:>1 <:>2 <:>3 <:>4 <:>5"
        it "" do
           bound "1 2 AB 3 4 M 5"  `shouldBe` "<:A>1 <:A>2 <B:A>AB <B:M>3 <B:M>4 <M:M>M <M:>5"

        
-- * For simple shelf
exec0 = execWH (emptyWarehouse $ fromGregorian 2025 02 21) . (withBoxOrientations ors )
    where ors = parseOrientationRule [] "!'"

addSorted :: [Text] -> [Text] -> WH [Text] s
addSorted boxes newBoxes =  do
        makeShelves $ words "S T pending"
        boxes <- makeBoxesWithPartition PRightOnly boxes
        newBoxes <- map fst <$> makeBoxesWithPartition PSortedOverlap newBoxes
        showBoxWithPosition newBoxes
             
showBoxWithPosition :: [Box s] -> WH [Text] s
showBoxWithPosition boxes = mapM (\(box,i) -> expandAttribute box i "${boxname} ${shelfname} ${position-spec}" ) 
                                 (zip boxes [1..])


shouldWH :: Ord a => Show a => WH [a] RealWorld  -> [a] -> IO ()
shouldWH action expected =  fmap sort (exec0 action) `shouldReturn` sort expected
     
      
