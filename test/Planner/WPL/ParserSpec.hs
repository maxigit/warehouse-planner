{-# LANGUAGE OverloadedLists #-}
module Planner.WPL.ParserSpec where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.WPL.Parser
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector (parseBoxSelector, parseShelfSelector, parseSelector)
import WarehousePlanner.Type
import Text.Megaparsec qualified as P
import GHC.Exts -- to write maybe as list
import Data.Char (isUpper)
import Data.Either (isLeft, isRight)

instance IsString BoxSelector where fromString = parseBoxSelector . pack 
-- instance IsString ShelfSelector where fromString = ShelfSelector SelectAnything . parseSelector . pack
instance IsString ShelfSelector where fromString = parseShelfSelector . pack . ('/':)

instance IsString (CSelector ShelfSelector) where fromString = CSelector . fromString
instance IsString (CSelector BoxSelector) where fromString = CSelector . fromString

instance IsString Statement where
   fromString s@(c:_) | isUpper c || c `elem` ("/#*" :: String) = Action . SelectBoxes $ fromString s
   fromString s = error $ s <> " should start with an uppercase"

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
        parse txt `shouldBe` Right [result]

parseAs' :: [Text] -> Statement -> IO ()
parseAs' txts stmt = parse' txts `shouldBe` Right [stmt]

sameAs' :: [Text ] -> [Text] -> IO ()
sameAs' xs ys = do
  let x = parse "left" xs
      y = parse "right" ys
  x `shouldSatisfy` isRight
  x `shouldBe` y
  where parse desc ts = P.parse wplParser desc (intercalate "\n" ts) 

parse :: Text -> Either _ [Statement]
parse = P.parse wplParser "test"
parse' :: [Text] -> Either _ [Statement]
parse' = parse . unlines
-- * shortcut
a = Action
m = Action . Move Nothing Nothing []
ss = SelectShelves . CSelector . parseShelfSelector
sb = SelectBoxes

-- * Spec
pureSpec :: Spec
pureSpec = fdescribe "Parsing" do
   it "parses boxes selections" do
      "BOXES" `parseAs` "BOXES"
   it "parses shelves sections" do
      "/SHELF" `parseAs` Action (SelectShelves "SHELF")
   it "parses boxes in shelves sections" do
      "in SHELF" `parseAs` "/SHELF"
   it "parses shelves containing boxes sections" do
      "with BOXES" `parseAs` Action (SelectShelves $ CSelector $ parseShelfSelector  "BOXES")
   it "parses simple move" do
      "BOXES to SHELF" `parseAs` ("BOXES" `Then` (m "SHELF"))
   xit "parses oneliner case" do
      "BOXES | A | B" `parseAs` ("BOXES" `Then` (Cases [ Case "A" []
                                                                            , Case "B" []
                                                                            ]))
   it "parses cases with initial break" do
      --  123456789
      [  "BOXES"
       , "      | A to S1"
       , "      | B to S2"
       ] `parseAs'` ("BOXES" `Then`
                           Cases [ "A" `Case` [m "S1"]
                                 , "B" `Case` [m "S2"]
                                 ]
                   )
   it "parses cases with initial break" do
      --  123456789
      [  "BOXES"
       , "  | A to S1"
       , "  | B to S2"
       ] `sameAs'` [  "BOXES"
                   , "      | A to S1"
                   , "      | B to S2"
                   ]
   it "parses cases with basic indentation" do
      --  123456789
      [  "BOXES | A to S1"
       , "      | B to S2"
       ] `parseAs'` ("BOXES" `Then`
                           Cases [ "A" `Case` [m "S1"]
                                 , "B" `Case` [m "S2"]
                                 ]
                   )

   it "parses cases with nested indentation" do
      [  "BOXES | A | to S1"
       , "          | to S2"
       , "      | B to S2"
       ] `parseAs'` ("BOXES" `Then`
                           Cases [ "A" `Case`
                                      [Cases [ m "S1" `Case` []
                                             , m "S2" `Case` []
                                             ]
                                      ]
                                 , "B" `Case` [m "S2"]
                                 ]
                   )
   it "parses with more nested cases " do
      [  "BOXES | A | to S1 | to X"
       , "                  | Y    "
       , "          | to S2    "
       , "      | B to S2"
       ] `parseAs'` ("BOXES" `Then`
                           Cases [ "A" `Case`
                                      [ Cases [ m "S1" `Case`
                                                                             [ Cases [ m "X" `Case` []
                                                                                     , "Y" `Case` [] 
                                                                                     ]
                                                                             ]
                                              , m "S2" `Case` []
                                              ]
                                      ]
                                 , "B" `Case` [ m "S2"]
                                 ]
                   )
   it "parses ors with nested indentation" do
      [  "BOXES  A to S1"
       , "         to S2"
       , "       B to S2"
       ] `parseAs'` ("BOXES" `Then`
                           Ors [ "A" `Then`
                                      Ors [ m "S1"
                                          , m "S2"
                                          ]
                               , "B" `Then` m "S2"
                               ]
                   )
   it "parses TAM with location" $ do
      "tam loc" `parseAs` Action (TagAndMove "loc" [])
   it "parses TAM with tag" do
      "tam #tag" `parseAs` Action (TagAndMove "#tag" [])
   it "parses TAM with orientations" do
      "tam loc with |= " `parseAs` Action (TagAndMove "loc" [ OrientationStrategy {osOrientations = readOrientation '|'
                                                                                   , osMinDepth = 0
                                                                                   , osMaxDepth = 1
                                                                                   , osMaxLenght = Nothing
                                                                                   , osMaxHeight = Nothing
                                                                                   , osUseDiagonal = True
                                                                                   }
                                                             , OrientationStrategy {osOrientations = readOrientation '='
                                                                                   , osMinDepth = 0
                                                                                   , osMaxDepth = 1
                                                                                   , osMaxLenght = Nothing
                                                                                   , osMaxHeight = Nothing
                                                                                   , osUseDiagonal = True
                                                                                   }
                                                             ])
   context "uppercase" do
   -- only command should start with a lower case unless starting with '
     it "rejects lower case selector" do
        parse "a" `shouldSatisfy` isLeft
     it "accept escaped lower case selector" do
        "`a" `parseAs` Action (SelectBoxes "a")
     it "accept  lower case selector as command argument" do
        "with boxes" `parseAs` Action (SelectShelves $ CSelector $ parseShelfSelector  "boxes")
        




                  
     
