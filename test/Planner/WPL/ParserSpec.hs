{-# LANGUAGE OverloadedLists #-}
module Planner.WPL.ParserSpec where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.WPL.Parser
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector (parseBoxSelector, parseShelfSelector)
import WarehousePlanner.Type(BoxSelector, ShelfSelector)
import Text.Megaparsec qualified as P
import GHC.Exts -- to write maybe as list

instance IsString BoxSelector where fromString = parseBoxSelector . pack 
instance IsString ShelfSelector where fromString = parseShelfSelector . pack

instance IsList (Maybe a) where
     type Item (Maybe a) = a
     toList = maybe [] pure
     fromList = headMay

spec :: Spec
spec = parallel pureSpec

parseAs :: Text -> Statement -> IO ()
parseAs txt result = P.parse wplParser "test" txt `shouldBe` Right result
-- parseAs txt _ = P.parseTest wplParser txt

pureSpec :: Spec
pureSpec = describe "Parsing" do
   it "A (B)" do
     "A (B)\
     \  ~C\
     \" `parseAs` (Action (SelectBoxes "A")
                  )
                  
     
