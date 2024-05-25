{-# LANGUAGE OverloadedLists #-} module Planner.WPL.ParserSpec where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.WPL.Parser
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector (parseBoxSelector, parseShelfSelector, parseSelector)
import WarehousePlanner.Type
import Text.Megaparsec qualified as P
import GHC.Exts -- to write maybe as list

instance IsString BoxSelector where fromString = parseBoxSelector . pack 
-- instance IsString ShelfSelector where fromString = ShelfSelector SelectAnything . parseSelector . pack
instance IsString ShelfSelector where fromString = parseShelfSelector . pack . ('/':)

instance IsList (Maybe a) where
     type Item (Maybe a) = a
     toList = maybe [] pure
     fromList = headMay

spec :: Spec
spec = parallel pureSpec

parseAs :: Text -> Statement -> IO ()
-- parseAs txt result = P.parse wplParser "test" txt `shouldBe` Right [result]
parseAs txt result = do
        -- P.parseTest wplParser ("\n" <> txt)
        P.parse wplParser "test" txt `shouldBe` Right [result]

parseAs' :: [Text] -> Statement -> IO ()
parseAs' txts stmt = parseAs (intercalate "\n" txts) stmt

pureSpec :: Spec
pureSpec = describe "Parsing" do
   it "parses boxes selections" do
      "BOXES" `parseAs` Action (SelectBoxes "BOXES")
   it "parses shelf sections" do
      "/SHELF" `parseAs` Action (SelectShelves "SHELF")
   it "parses simple move" do
      "BOXES to SHELF" `parseAs` (Action (SelectBoxes "BOXES") `Then` (Action (Move Nothing ["SHELF"])))
   xit "parses oneliner case" do
      "BOXES | A | B" `parseAs` (Action (SelectBoxes "BOXES") `Then` (Cases [ Case (Action (SelectBoxes "A")) []
                                                                            , Case (Action (SelectBoxes "B")) []
                                                                            ]))
   it "parses cases with initial break" do
      --  123456789
      [  "BOXES"
       ,  "      | A to S1"
       , "      | B to S2"
       ] `parseAs'` (Action (SelectBoxes "BOXES") `Then`
                           Cases [ Action (SelectBoxes "A") `Case` [Action (Move Nothing ["S1"])]
                                 , Action (SelectBoxes "B") `Case` [Action (Move Nothing ["S2"])]
                                 ]
                   )
   it "parses cases with basic indentation" do
      --  123456789
      [  "BOXES | A to S1"
       , "      | B to S2"
       ] `parseAs'` (Action (SelectBoxes "BOXES") `Then`
                           Cases [ Action (SelectBoxes "A") `Case` [Action (Move Nothing ["S1"])]
                                 , Action (SelectBoxes "B") `Case` [Action (Move Nothing ["S2"])]
                                 ]
                   )

   it "parses cases with nested indentation" do
      [  "BOXES | A | to S1"
       , "          | to S2"
       , "      | B to S2"
       ] `parseAs'` (Action (SelectBoxes "BOXES") `Then`
                           Cases [ Action (SelectBoxes "A") `Case`
                                      [Cases [ Action (Move Nothing ["S1"]) `Case` []
                                             , Action (Move Nothing ["S2"]) `Case` []
                                             ]
                                      ]
                                 , Action (SelectBoxes "B") `Case` [Action (Move Nothing ["S2"])]
                                 ]
                   )
   it "parses with more nested cases " do
      [  "BOXES | A | to S1 | to X"
       , "                  | Y    "
       , "          | to S2    "
       , "      | B to S2"
       ] `parseAs'` (Action (SelectBoxes "BOXES") `Then`
                           Cases [ Action (SelectBoxes "A") `Case`
                                      [ Cases [ Action (Move Nothing ["S1"]) `Case`
                                                                             [ Cases [ Action (Move Nothing ["X"]) `Case` []
                                                                                     , Action (SelectBoxes "Y") `Case` [] 
                                                                                     ]
                                                                             ]
                                              , Action (Move Nothing ["S2"]) `Case` []
                                              ]
                                      ]
                                 , Action (SelectBoxes "B") `Case` [ Action (Move Nothing ["S2"])]
                                 ]
                   )
   it "parses ors with nested indentation" do
      [  "BOXES  A to S1"
       , "         to S2"
       , "       B to S2"
       ] `parseAs'` (Action (SelectBoxes "BOXES") `Then`
                           Ors [ Action (SelectBoxes "A") `Then`
                                      Ors [ Action (Move Nothing ["S1"])
                                             , Action (Move Nothing ["S2"])
                                          ]
                               , Action (SelectBoxes "B") `Then` Action (Move Nothing ["S2"])
                               ]
                   )


                  
     
