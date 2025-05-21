{-# LANGUAGE OverloadedLists #-}
module Planner.WPL.ParserISpec where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.WPL.ParserI
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector (parseBoxSelector, parseShelfSelector, parseSelector)
import WarehousePlanner.Type
import Text.Megaparsec qualified as P
import Text.Megaparsec.Debug qualified as P
import GHC.Exts -- to^ write maybe as list
import Data.Char (isUpper)
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty as NE

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
        parse txt `shouldParse` [result]

parseAs' :: [Text] -> Statement -> IO ()
parseAs' txts stmt = parse' txts `shouldParse` [stmt]

shouldParse actual expected = case actual of
           Right act | act == expected -> return ()
           Right act -> act `shouldBe` expected
           Left e -> expectationFailure $ P.errorBundlePretty e

sameAs' :: [Text ] -> [Text] -> IO ()
sameAs' xs ys = do
  let x = parse "left" xs
      y = parse "right" ys
  x `shouldSatisfy` isRight
  x `shouldBe` y
  where parse desc ts = P.parse wplParserI desc (intercalate "\n" ts) 

parse :: Text -> Either _ [Statement]
-- parse = P.parse (P.dbg "WPL" wplParser) "test"
parse = P.parse wplParserI "test"

parse' :: [Text] -> Either _ [Statement]
parse' = parse . unlines
-- * shortcut
a = Action
m = Action . (Move Nothing Nothing []) . pure . (ExitOnTop,)
ss = SelectShelves . CSelector . parseShelfSelector
sb = SelectBoxes

-- * Spec
pureSpec :: Spec
pureSpec = describe "Parsing" do
   it "parses boxes selections" do
      "BOXES" `parseAs` "BOXES"
   it "parses shelves sections" do
      "/SHELF" `parseAs` Action (SelectShelves "SHELF")
   it "parses boxes in shelves sections" do
      "in SHELF" `parseAs` "/SHELF"
   it "parses shelves containing boxes sections" do
      "with BOXES" `parseAs` Action (SelectShelves $ CSelector $ parseShelfSelector  "BOXES")
   it "parses simple move" do
      "BOXES to^ SHELF" `parseAs` ("BOXES" `Then` (m "SHELF"))
   it "parses oneliner case" do
      "BOXES | A | B" `parseAs` ("BOXES" `Then`
                                          (Cases [ Case "A" [Cases [Case "B" []]]]
                                          )
                                )
   it "parses cases with initial break (bigger)" do
      --  12345678901234
      [  "BOXES"
       , "      | A to^ S1"
       , "      | B to^ S2"
       ] `parseAs'` ("BOXES" `Then`
                           Cases [ "A" `Case` [m "S1"]
                                 , "B" `Case` [m "S2"]
                                 ]
                   )
   it "parses cases with initial break (smaller)" do
      --  123456789
      [  "BOXES"
       , "  | A to^ S1"
       , "  | B to^ S2"
       ] `sameAs'` [  "BOXES"
                   , "      | A to^ S1"
                   , "      | B to^ S2"
                   ]
   it "parses cases with basic indentation" do
      --  123456789
      [  "BOXES | A to^ S1"
       , "      | B to^ S2"
       ] `parseAs'` ("BOXES" `Then`
                           Cases [ "A" `Case` [m "S1"]
                                 , "B" `Case` [m "S2"]
                                 ]
                   )

   it "parses cases with nested indentation" do
      --  12345678901234
      [  "BOXES | A | to^ S1"
       , "          | to^ S2"
       , "      | B to^ S2"
       ] `parseAs'` ("BOXES" `Then`
                           Cases [ "A" `Case`
                                      [Cases [ m "S1" `Case` []
                                             , m "S2" `Case` []
                                             ]
                                      ] , "B" `Case` [m "S2"]
                                 ]
                   )
   it "parses with more nested cases " do
      [  "BOXES | A | to^ S1 | to^ X"
       , "                   | Y    "
       , "          | to^ S2    "
       , "      | B to^ S2"
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
      -- ((BOXES  (A to^ S1)
      --            to^ S2)
      --          B to^ S2)
      [  "BOXES  A to^ S1"
       , "         to^ S2"
       , "       B to^ S2"
       ] `parseAs'`  Then (Then ("BOXES" `Then` ("A" `Then`  m "S1")) -- first line
                                ( m "S2" )                                  -- second block
                          )
                          ("B" `Then` m "S2")
       --                     Ors [ "A" `Then`
       --                                Ors [ m "S1"
       --                                    , m "S2"
       --                                    ]
       --                         , "B" `Then` m "S2"
       --                         ]
   it "parses ors and cases " do
       [ "A"
        ,"  B"
        ,"  | X"
        ,"  | Y"
        ] `parseAs'` Then "A"  ( Ors [ "B"
                                     , Cases [ Case "X" []
                                             , Case "Y" []
                                             ]
                                     ]
                               )
   it "bug" do
     let move s = Action $ Move Nothing Nothing [] (NE.singleton (ExitLeft, s))
     [   "#batch=M "
       , "     | #-first to> C2"
       , "     | #first tam @C1"
       , "    ------------------------------------------------"
       , "     | #first"
       , "         | M52L04FC"
       , "            .~ M22L08DG"
       , "                   to> pending"
       , "                   # | #a | to> E07.06/2"
       , "                     | to> E07.06/3"
       , "                     | to> error"
       , "            | to> E07.06/1"
       , "            | to> E07.06/3"
       , "            | to> error"
       ] `parseAs'`
          ( Then "#batch=M" 
                ( Cases [ Case "#!first"[ move "C2" ]
                        , Case "#first" [ Action (TagAndMove "@C1" []) ]
                        , Case "#first"
                            [ Cases [ Case "M52L04FC"
                                             [ Ors [ Then ( Then (Action (SelectBoxes Root)) "M22L08DG")
                                                          (Ors [ move "pending"
                                                               , Then "#"
                                                                      ( Cases [ Case "#a" [  Cases [ Case (move "E07.06/2") [] ] ]
                                                                              , Case (move "E07.06/3") Nothing
                                                                              , Case (move "error") Nothing
                                                                              ]
                                                                      )
                                                               ]
                                                          )
                                                   , Cases [ Case (move "E07.06/1") Nothing
                                                           , Case (move "E07.06/3") Nothing
                                                           , Case (move "error") Nothing
                                                           ]
                                                   ]
                                             ]
                                    ]
                            ]
                       ]
                )
          )



   context "new" do
            it "atom" do
               ["A" ] `parseAs'` "A"
            it "atom" do
               "A" `parseAs` "A"
               "A  " `parseAs` "A"
            it "parses naked cases" do
               [ "| A"
                ,"| B"
                ] `parseAs'` Cases [ Case "A" []
                                   , Case "B" []
                                   ]
            it "parses cases without spaces" do
               [ "|A"
                ] `parseAs'` Cases [ Case "A" []
                                   ]
            it "parse mix of cases" do
               parse' [ "A"
                ,"|B"
                ,"|C"
                ,"D"
                ] `shouldParse` [ "A"
                                 , Cases [ Case "B" []
                                         , Case "C" []
                                         ]
                                 , "D"
                                 ]
            context "then" do
               it "single line" do
                 "A B" `parseAs`  Then "A" "B"
               it "broke line" do
                 [ "A "
                  ," B"
                  ] `parseAs'` Then "A" "B"
               context "with ors" do
                       it "1" do
                         [ "A B C"
                          ," D"
                          ] `parseAs'` Then ("A" `Then` ("B" `Then` "C"))
                                            "D"
                       it "2" do
                         [ "A"
                          ,"    B"
                          ,"    C"
                          ] `parseAs'` Then "A" ( Ors [ "B"
                                                      , "C"
                                                      ]
                                                )
                       it "3" do
                         [ "A"
                          ,"  B"
                          ,"    C"
                          ,"  D"
                          ] `parseAs'` Then "A" ( Ors [ "B" `Then` "C"
                                                      , "D"
                                                      ]
                                                )
                       it "4" do
                         [ "Axxxxx "
                          ,"     B"
                          ,"  C"
                          ,"  D"
                           ] `parseAs'` Then (Then "Axxxxx"
                                                   "B")
                                             (Ors ["C", "D"])
                       it "5" do
                         [ "A A2 A3"
                          ,"  B"
                          ,"    C"
                          ,"  D"
                          ] `parseAs'` Then ("A" `Then` ( "A2" `Then` "A3"))
                                            ( Ors [ "B" `Then` "C"
                                                  , "D"
                                                  ]
                                            )
               it "with &" do
                   "A & B & C" `parseAs` Then "A" (Then "B" "C")
               it "with indented &" do
                   [ "A & B"
                    ,"  & C"
                    ,"  & D"
                    ] `parseAs'` Then "A" ("B" `Then` ("C" `Then` "D"))

            context "multi" do
               it "one per line" do
                parse' [ "A"
                 ,"B"
                 ] `shouldParse` ["A"
                                 , "B"
                                 ]
               it "with cases" do
                parse' [ "A | A2"
                       , "  | A3"
                        ,"B B2"
                 ] `shouldParse` ["A" `Then` (Cases [ Case "A2" []
                                                    , Case "A3" []
                                                    ]
                                             )
                                 , "B" `Then` "B2"
                                 ]
               it "with cases wi" do
                --        123456789
                parse' [ "X"
                       , "    A | A2"
                       , "      | A3"
                       , "    B B2"
                 ] `shouldParse` [Then "X" $ Ors ["A" `Then` (Cases [ Case "A2" []
                                                    , Case "A3" []
                                                    ]
                                             )
                                 , "B" `Then` "B2"
                                 ]]
               it "A B Z" do
                parse' [ "A"
                       , "  B"
                       , "Z"
                       ] `shouldParse` ["A" `Then` "B"
                                       , "Z"
                                       ]
               it "A B C D" do
                parse' [ "A"
                       , "  B"
                       , "    C"
                       , "  D"
                       ] `shouldParse` ["A" `Then` Ors ["B" `Then` "C"
                                                       , "D"
                                                       ]
                                       ]
   context "foreach" do
      it "one line" do
         ["/A foreach:shelf B C"
          ] `parseAs'` (Then (Action $ SelectShelves "A")
                             $ ForeachShelf ("B" `Then` "C")
                       )
      it "uses full line only" do
         [ "/A"
          ,"   foreach:shelf B"
          ,"        C"
          ] `parseAs'` (Then (Action $ SelectShelves "A")
                             $ Then (ForeachShelf "B") 
                                    "C"
                       )
      it "return two statements" do
         -- foreach should not consume the C
         parse' [ "/A"
                , "   foreach:shelf B"
                , "C"
                ] `shouldParse` [Then (Action $ SelectShelves "A")
                             $ ForeachShelf "B"
                             , "C"
                             ]
      it "uses indented block after newline" do
         parse' [ "/A"
                , "   foreach:shelf"
                , "      B"
                , "      C"
                ] `shouldParse` [Then (Action $ SelectShelves "A")
                                $ ForeachShelf $ Ors ["B" , "C" ]
                                ]
      it "uses indented block same line" do
         parse' [ "/A"
                , "   foreach:shelf B"
                , "                 C"
                ] `shouldParse` [Then (Action $ SelectShelves "A")
                             $ ForeachShelf $ Ors ["B" , "C" ]
                              ]
      it "don't use next line" do
         parse' ["/A foreach:shelf trace:count before in:shelves tam @"
          , "B"]
            `shouldParse` [ Then (Action $ SelectShelves "A")
                                 ( ForeachShelf (Then (Action $ TraceCount "before" )
                                                      (Action (SelectBoxes  CCrossSelection) `Then` (Action $ TagAndMove "@" []))
                                                )
                                 )

                          , "B"
                          ]
      it "don't use next line in block" do
         parse' ["/A || foreach:shelf GO"
                ,"   || B"]
            `shouldParse` [ Then (Action $ SelectShelves "A")
                                 (Cases [ Case ( ForeachShelf "GO") []
                                        , Case "B" []
                                        ]
                                 )
                          ]
      it "|| & ||" do
        let sub =       Cases [Case (Then (Cases [ Case "A" []
                                                 , Case "B" []
                                                 ]
                                           )
                                           "C"
                                   ) [] 
                              ]

        [ "| Main "
         ,"   Root "
         ,"   || & || A"
         ,"        || B"
         ,"      & C"
         ] `parseAs'` Cases [Case "Main"  [ Ors [ "Root"
                                                , sub
                                                ]
                                          ]
                            ]
      it "|| & || (2)" do
       [  "* |  & | A#'3" -- collect A-2
        , "       | B#'1" -- and B-1
        , "     & #id=1"  -- only B-1#1
        , "  || A#'2"     -- add A-3
        ] `parseAs'` Then "*"
                         (Cases [ Case  (Then (Cases [ (Case "A#'3" [])
                                                     , (Case "B#'1" [])
                                                     ]
                                              )
                                              "#id=1"
                                        ) 
                                        []
                                -- "  || A#'3"     -- add A-3
                                , Case "A#'2" []
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
   context "selectors" do
      it "1" do
         ".~" `parseAs` (Action (SelectBoxes Root))
      it "2" do
         ".~~" `parseAs` (Action (SelectBoxes $ CSelectorAnd Root Parent) )
      it "3" do
         ".~~A" `parseAs` (Action (SelectBoxes $ CSelectorAnd Root $ CSelectorAnd  Parent "A") )
      it "4" do
         ".~  A" `parseAs` Then (Action (SelectBoxes Root)) "A"
      context "shelf case" do
         it "" do
            [ "& // A B"
             ,"  /  C"
             ,"& GO"
             ] `parseAs'` Then (ShelfCases [ ShelfCase (Action (SelectShelves "A") `Then` Action (SelectShelves "B")) []
                                           , ShelfCase (Action (SelectShelves "C")) []
                                           ]
                               )
                               ( "GO")
   context "bugs strategsies" do
     it "1" do
        [ "A "
         ,"  .~ B | C"
         ,"       | D"
         ,"  E | F"
         ,"    | G"
         ]`parseAs'` ("A" `Then` Ors [ (Action (SelectBoxes Root)) `Then` ("B" `Then` Cases [Case "C" [], Case "D" []])
                                     , "E"  `Then` Cases [Case "F" [], Case "G" []]
                                     ]
                     )
     it "2" do
       let z = "Z" `Then` Cases [ Case (m "E07.06/1") []
                                , Case (m "E07.06/3") []
                                , Case (m "error") []
                                ] 
       let y = "Y" `Then` Cases [ Case (Action $ SelectBoxes "^=1")
                                       [Cases [ Case (m "E07.06/2") [] ]
                                       ]
                                , Case (m "E07.06/3") []
                                , Case (m "error") []
                                ] 
           c = (Action (SelectBoxes Root) `Then` "C") `Then` Ors [m "pending", y]
       [ "A"
         ,"       | B"
         ,"          .~ C"
         ,"                 to^ pending"
         ,"                 Y | ^1 | to^ E07.06/2"
         ,"                   | to^ E07.06/3"
         ,"                   | to^ error"
         ,"          Z | to^ E07.06/1"
         ,"            | to^ E07.06/3"
         ,"            | to^ error"
        ]`parseAs'` ( "A"  `Then` (Cases [Case "B" [Ors [c, z]]])
                    )
        




                  
     
