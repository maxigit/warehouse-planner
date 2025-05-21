{-# OPTIONS_GHC -fconstraint-solver-iterations=100 #-}
{-# LANGUAGE OverlappingInstances #-}
module Planner.WPL.ParserSpec where

import ClassyPrelude
import Test.Hspec hiding (Selector)
import WarehousePlanner.WPL.Parser
import WarehousePlanner.WPL.Types
import WarehousePlanner.Type
import WarehousePlanner.Expr
import WarehousePlanner.WPL.PrettyPrint
import WarehousePlanner.Selector (parseBoxSelector, printTagSelector, parseTagSelector, parseMatchPattern)
import Text.Megaparsec qualified as P
import Test.QuickCheck
import Data.Text (Text, strip)
import Data.List.NonEmpty (NonEmpty(..))
import Test.QuickCheck.Random (mkQCGen)
import Test.Hspec.QuickCheck(modifyArgs)
import Data.Char (isSpace)
import WarehousePlanner.Base 

import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary.Generic

deriving instance Generic Statement
instance Arbitrary Statement where
   arbitrary = rebalance <$> 
               frequency [ (10, atom)
                     , (1, Then <$> atom <*> arbitrary)
                     , (1, PassThrought <$> atom)
                     , (1, ForeachShelf <$> arbitrary)
                     , (1, ForeachBox <$> arbitrary <*> arbitrary )
                     , (1, ForeachDo <$> arbitrary <*> arbitrary )
                     , (1, PrettyPrint <$> arbitrary <*> arbitrary )
                     , (1, do 
                        -- cs <- arbitrary
                        c <- arbitrary
                        cs' <- scale (min 3) $ listOf arbitrary
                        let cs = c:| cs'
                        elements [  Cases $ fmap (\st -> Case st Nothing ) cs
                                 , ShelfCases $ fmap (flip ShelfCase Nothing) cs
                                 ]
                        )
                     , (1, do -- generate  at least 2 in Ors
                         a <- arbitrary `suchThat` \case Ors _ -> False; PassThrought _ -> False ; _ -> True
                         b <- arbitrary
                         cs <- return [] -- scale (min 2) $ listOf arbitrary
                         return $ Ors (a :| b : cs)
                         )

                     ]
             where atom = oneof [ Action <$> genericArbitrary 
                                ]
                   rebalance = \case
                      Then (Then a b) c -> Then a $ rebalance (Then b c)
                      Ors (a :| []) -> rebalance a
                      a -> a
   shrink = shrinkNothing

deriving instance Generic Command
instance Arbitrary Command where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic Case
instance Arbitrary Case where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic ShelfCase
instance Arbitrary ShelfCase where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic BoxSelector
instance Arbitrary BoxSelector where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic ShelfSelector
instance Arbitrary ShelfSelector where
   arbitrary = genericArbitrary
   shrink = genericShrink

instance {-# OVERLAPPING #-} Arbitrary (Maybe ShelfSelector) where
   arbitrary = do
                p <- arbitrary
                return case p of
                   SelectAllShelves -> Nothing
                   s -> Just s
instance {-# OVERLAPPING #-} Arbitrary (Maybe BoxSelector) where
   arbitrary = do
                p <- arbitrary
                return case p of
                   SelectAllBoxes -> Nothing
                   s -> Just s

deriving instance Generic (Selector a)
instance Arbitrary (Selector a) where
   arbitrary = do 
                 sel <- genericArbitrary
                 return $ case sel of 
                          SelectAnything -> SelectAnything
                          _ -> sel
   shrink = shrinkNothing -- genericShrink

deriving instance Generic s => Generic (CSelector s)
instance (Generic s, Arbitrary s) => Arbitrary (CSelector s) where
   arbitrary = oneof $ atom <> [ CSelector  <$> arbitrary
                               , do
                                  a <- oneof atom
                                  b <- arbitrary
                                  return $ CSelectorAnd a b
                               ]
             where atom = map return [ SwapContext
                                     , Parent
                                     , Root
                                     , CUseContext
                                     ]
   shrink = genericShrink

-- deriving instance Generic (CSelector (ShelfSelector))
-- instance Arbitrary (CSelector (ShelfSelector )) where
--    arbitrary = genericArbitrary
--    shrink = genericShrink

deriving instance Generic Limit
instance Arbitrary Limit where
   arbitrary = do
                 liStart <- oneof [return Nothing, Just <$> chooseInt (1, 3) ]
                 liEnd <- oneof [return Nothing, Just <$> chooseInt (1, 3) ]
                 liOrderingKey <- arbitrary
                 liUseBase <- arbitrary
                 return case Limit{..} of
                          NoLimit -> NoLimit
                          l -> l
   shrink = shrinkNothing

deriving instance Generic SortingOrder
instance Arbitrary SortingOrder where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic OrderingKey
instance Arbitrary OrderingKey where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic (BoxNumberSelector)
instance Arbitrary (BoxNumberSelector) where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic (Expr Text)
instance Arbitrary (Expr Text) where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic (NameSelector a)
instance Arbitrary (NameSelector a) where
   arbitrary = do 
                 n <- oneof [NameMatches <$> arbitrary, NameDoesNotMatch <$> arbitrary ]
                 
                 case n of
                   AnyNames -> return AnyNames
                   NameDoesNotMatch (MatchAnything:_) ->  arbitrary
                   _ -> return n
                        
   shrink = shrinkNothing -- genericShrink

deriving instance Generic (TagSelector a)
instance Arbitrary (TagSelector a) where
   arbitrary = do
                 -- oneof [ 
                   a <- genericArbitrary
                   case a of
                      TagHasKey MatchAnything -> arbitrary
                      TagIsKey MatchAnything -> arbitrary
                      TagHasValues vs | matchAll vs -> arbitrary
                      TagHasNotValues vs | matchAll vs -> arbitrary
                      TagHasKeyAndValues MatchAnything _ -> arbitrary
                      TagHasKeyAndValues k vs | matchAll vs -> return $ TagHasKey k
                      TagIsKeyAndValues MatchAnything _ -> arbitrary
                      TagIsKeyAndValues k vs | matchAll vs -> return $ TagIsKey k
                      TagHasKeyAndNotValues MatchAnything _ -> arbitrary
                      TagHasKeyAndNotValues _ vs | matchAll vs -> arbitrary
                      TagHasValues vs | matchAll vs -> arbitrary
                      _ -> return a
               where matchAll = all (== (VMatch MatchAnything))
   shrink = shrinkNothing
   

deriving instance Generic ValuePattern
instance Arbitrary ValuePattern where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic OJustify
instance Arbitrary OJustify where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic OrientationStrategy
instance Arbitrary OrientationStrategy where
   arbitrary = do
                osOrientations <- arbitrary
                osMinDepth <- chooseInt (1,9) `suchThat` (/=1)
                osMaxDepth <- chooseInt (max 1 osMinDepth, 9) `suchThat` (/=1)
                osMaxLenght <- oneof [return Nothing, Just <$> chooseInt (1,9) `suchThat` (/=1) ]
                osMaxHeight <- oneof [return Nothing, Just <$> chooseInt (1,9) `suchThat` (/=1) ]
                osUseDiagonal <- return False
                return $ OrientationStrategy{..}
   -- shrink = genericShrink
   shrink _ = []
   

instance {-# OVERLAPPING #-} Arbitrary [OrientationStrategy] where
   arbitrary = do 
               -- make sure diag is consistent
               ostrat <- arbitrary
               let ori = osOrientations ostrat
               elements [ [ ostrat { osUseDiagonal = False } ]
                        , [ ostrat { osUseDiagonal = True, osOrientations = o } 
                          | o <- [ori, rotateO ori ]
                          ] 
                        , [ ostrat { osUseDiagonal = False, osOrientations = o } 
                          | o <- [ori, rotateO ori ]
                          ] 
                        ]
   shrink = shrinkNothing

deriving instance Generic BoxMode
instance Arbitrary BoxMode where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic RangeBoundary
instance Arbitrary RangeBoundary where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic ExitMode
instance Arbitrary ExitMode where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic Direction
instance Arbitrary Direction where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic Orientation
instance Arbitrary Orientation where
   arbitrary = elements [ up, tiltedForward, tiltedRight, tiltedFR, rotatedSide, rotatedUp ]
   shrink = genericShrink

deriving instance Generic PartitionMode
instance Arbitrary PartitionMode where
   arbitrary = do 
             pmode <- genericArbitrary
             return $ reOr $ case pmode of
               -- make sure c >= 1
               PCorner c | c<=0 -> PCorner (1-c)
               _ -> pmode
             where reOr = \case 
                         POr (POr a b) c -> POr a $ reOr $ POr c b
                         p -> p
   shrink = genericShrink

deriving instance Generic (TagOperationF Text)
instance Arbitrary (TagOperationF Text) where
   arbitrary = genericArbitrary
   shrink = genericShrink

deriving instance Generic MatchPattern
instance Arbitrary MatchPattern where
   arbitrary = do
                  -- oneof [fmap MatchFull arbitrary, return MatchAnything]
                  elements [MatchFull "pat", MatchAnything, MatchFull "name", parseMatchPattern "[WE]01.02/3", parseMatchPattern "*A"   ]
   shrink = shrinkNothing
   -- shrink = genericShrink
   
instance {-# OVERLAPPING #-} Arbitrary [MatchPattern] where
   arbitrary = take 2 <$> sublistOf [MatchFull pat
                                    | pat <- ["pat", "file"] <> map singleton (['a'..'z'] <> ['A'..'Z'])
                                    ] `suchThat` \xs -> length xs > 0 
   shrink = genericShrink

instance Arbitrary Text where
   arbitrary = elements ( [singleton c <> suf
                          | c <- ['A'..'Z']
                          , suf <- ["", "-X"]
                          ]
                        <> [ singleton w <> "0" <> tshow i <> ".0" <> tshow j
                           | w <- "WEM"
                           , i <- [1..9]
                           , j <- [1..9]
                           ]
                        <> ["done", "Error", "tag", "ghost"]
                        )

   shrink _ = []

instance Arbitrary  a => Arbitrary (NonEmpty a) where
    arbitrary = liftA2 (:|) arbitrary arbitrary
    shrink _ = []


spec :: Spec
-- spec = parallel pureSpec
spec = pureSpec

pureSpec = twoway >> parsing >> pretty

twoway = describe "pretty/parse" do
   it "parse orientation" do
     -- quickCheckWith stdArgs { replay = Just (mkQCGen 1501443633, 0)} $
      property \rule -> let s = showOrientationStratety rule
                        in (parseOrientationRule [] s, s) `shouldBe` ([rule], s)
   it "parse tags operation" do
      property \tagOp -> let s = printTagOperation tagOp
                        in (parseTagOperation s, s) `shouldBe` (tagOp, s)
   it "parse tags operations" do
      property \a -> let s = printTagOperations a
                        in (parseTagOperations s, s) `shouldBe` (a, s)
   it "parse tag selector" do
      property \a -> let s = printTagSelector a
                        in (parseTagSelector s, s) `shouldBe` (Just a, s)
   modifyArgs (\args ->   args
                            { maxSuccess= 100000, maxSize = 2
                            -- , replay = Just (mkQCGen 2069934030, 0)
                            }) $
      it "parse any prettyprint statement" do
                            property $ \statement -> let t = prettyShort statement
                                                         se = parse t
                                                      -- check if parse . pretty works
                                                      -- if not check pretty printing of both to display the pprint version
                                                      in if (se  == Right [statement ] ) 
                                                         then  return ()
                                                         -- else fmap (map prettyShort) (parse t) === Right ["", t]
                                                         else (fmap (map prettyShort) se, se)  `shouldBe` (Right [t], Right [statement])
                                                         --                                        force the error 
                                                         -- else parse t === Right []

-- | pretty print and collapse spaces for easier comparaison
prettyShort statement = pack $ go (unpack $ prettyWPL statement) where
  go (a:b:t) | isSpace a && isSpace b = go (' ':t)
  go (a:t) | isSpace a  = ' ' : go t
  go (a:t) = a : go t
  go "" = ""

parsing  = describe "parsing" do
   context "consecutive" do
       it "indented as Then" do
          parse "A\n B" `shouldBe` parse "A B"
       it "new line as Or" do
          parse  "A\nB" `shouldBe` fmap concat (traverse parse ["A", "B"])
   let texts = -- ["to> W", "to^ B"
               -- , "to^ orules:x1x2| W"
               -- , "to> boxes:B W"
               -- , "tag#bg=black"
               -- , "#bg=black { to> error }"
               -- , "#bg=black :A { to> error }"
               -- , "toggle #A#done#-error"
               -- , "t:b :message prop:${shelfname}"
               -- , "t:b :\"message\" prop:${shelfname}"
               -- , "shelf:tag#"
               -- , "empty:shelves=no"
               -- , "to> pmode:behind A"
               -- , "to> ~"
               -- , "tag#"
               -- , "shelf:split * { to> A }"
               -- , "/~"
               -- , "shelf:resize <useContext> { empty:boxes=yes }"
               -- , "shelf:full ~ { empty:boxes=yes }"
               -- , "shelf:resize <useContext> l:0.0 w:{M-X} h:{W01.02} { shelf:full ~ { empty:boxes=yes } }"
               -- , "orules+=x0x@"
               -- , "orules+=!x0x@" doesn't exist in real file as the ! disappear in the pretty printing
               --
               -- [ "tag# :.~ { tag# }"
               -- , "tag# :(orules+=<empty>) { tag# }"
               -- , "shelf:split -~ { tag# :(orules+=<empty>) { tag# } }"
               -- , "shelf:split -~ { tag# :(orules+=<empty>) { tag# :.~ { tag# } } }"
               -- , "shelf:split -~ { tag# :(orules+=<empty>) { tag# :.~ { tag# :~ { tag# :.~ { tag# } } } } }"
               -- "after ~ orules=!4x9:9x@' for:?!pat/pat"
              --  "trace:pretty :\"M06.07\" { foreach:box ~.~ { foreach:box ~.~ { ( shelf:full <useContext> { ; pmode=best } , shelf:resize <useContext> l:{E05.06} w:0.0 h:{M08.01} { /-~ } ) } } }":
               "/ [AB]" :
               "with:boxes" :
               []
   forM_ texts \t -> do
       it ("parses " <> unpack t) do
          case parse t of
           -- Right [s] -> pretty2 s `shouldBe` t
           Right ss -> map prettyShort ss `shouldBe` [t]
           Left e ->  expectationFailure $ P.errorBundlePretty e
   describe "options" do
      let t = Just @Text
      it "parses 2 option in correct order" do
        P.parse (o2 ["a"] ["b"] ) "o2" "a:hello b:message" `shouldBe` Right (t "hello", t "message")
      it "parses 2 option in incorrect order" do
        P.parse (o2 ["a"] ["b"] ) "o2" "b:message a:hello" `shouldBe` Right (t "hello", t "message")
      it "parses 2 option with nothing" do
        P.parse (o2 ["a"] ["b"] ) "o2" "" `shouldBe` Right (Nothing :: Maybe Text, Nothing :: Maybe Text)
      it "parses 3 option in correct order" do
        P.parse (o3 ["a"] ["b"] ["c"] ) "o3" "a:hello b:message c:third" `shouldBe` Right (t "hello", t "message", t "third")
      it "parses 3 option with nothing" do
         P.parse (o3 ["a"] ["b"] ["c"] ) "o3" "" `shouldBe` Right (Nothing :: Maybe Text, Nothing :: Maybe Text, Nothing :: Maybe Text)
pretty = describe "pretty" do
    it "layout" do
       content <- strip <$> readFileUtf8 "data/pretty.wpl"
       statements <- case parse content of
                       Right ss -> return ss
                       Left e -> error $ P.errorBundlePretty e

       let result = prettyWPLs statements
           -- parse as same result
       writeFileUtf8 "/tmp/pretty.wpl" result
       -- forM statements \st ->  do
       --     let r = prettyWPL st
       --     case parse r of
       --        Right ss ->  ss `shouldBe` [st]
       --        Left e -> error $ unpack $ unlines [ pack $ P.errorBundlePretty e, "when parsing", r ,"----" ]
       -- take 500 result `shouldBe` take 500 content 
       (r result <> "---") `shouldBe` (r content <> "\n---")


-- | spaces don't show on diff so we replace them by something visible
r = omap \case 
        ' ' -> '¬'
        c -> c
-- * util {{{
parse = P.parse wplParser "test"
