{-# Language BlockArguments #-}
{-# Language ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Planner.BaseSpec (spec) where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Base
import WarehousePlanner.Selector

import Planner.SpecUtil

spec :: Spec
spec = parallel pureSpec


pureSpec :: Spec
pureSpec = describe "tags operations" do
   context "parseTagOperations" do
     it "parse simple value" do
       parseTagOperations "a" `shouldBe` TagsOperations [("a", SetTag)] [] []
     it "parse complex values" do
       parseTagOperations "a=3#b" `shouldBe` TagsOperations [("a", SetValues ["3"])
                                                            ,("b", SetTag)
                                                            ] 
                                                            []
                                                            []
   context "modifyTags" do
     let b = TagIsKey (MatchFull "b")
         tags = [("a", ["3"]), ("b", []), ("point", ["x","y"])]
     it "set tag" do
         modifyTags (TagsOperations [("a", SetTag)] [] []) [] `shouldBe` Just [("a", [])]
     it "add values" do
         modifyTags (TagsOperations [("point", AddValue "z")] [] []) [("point", ["x", "y"])] `shouldBe` Just [("point", ["x", "y", "z"])]
     it "includes only b" do
       modifyTags (TagsOperations  [] [b] []) tags `shouldBe` Just [("b", [])]
     it "excludes b" do
       modifyTags (TagsOperations  [] [] [b]) tags `shouldBe` Just [("a", ["3"]), ("point", ["x", "y"])]




