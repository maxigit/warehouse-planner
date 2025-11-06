module Planner.ReportSpec (spec) where

import ClassyPrelude
import Test.Hspec hiding (shouldContain)
import WarehousePlanner.Report
import WarehousePlanner.Base

spec, pureSpec :: Spec
spec = parallel pureSpec

pureSpec = bestFitSpec

shouldContain xs_ exps_ =  do
  let xs = sortOn fst xs_
      exps   = sortOn fst exps_
  let keys = map fst exps
      filtered = filter (\(k,_) -> k `elem` keys) xs
  if length filtered == length exps
  then
     filtered `shouldBe` exps
  else if filtered == [] 
       then  -- displays keys
         map fst xs `shouldBe` map fst exps
       else 
         xs `shouldBe` exps -- show all keys

bestFitSpec = describe "best-fit" do
  {-
   /---------------+----------------+------------------+----------------------\
   |               :                | toFit > to Pick  |    to Fit <= to Pick |
   |               :                | to Pick < to Fit |    to Pick => to Fit |
   +---------------+----------------+------------------+----------------------+
   | to fit <= fit | to pick <= fit | 0: f >=F>P>= f   | B: f >= P > F        |
   |               | to pick >  fit | A: f > F > P     | C: P > f >= F        |
   +---------------+----------------+------------------+----------------------+
   | to fit > fit  | to pick <= fit | D: F > f >= P    | 0: P =F>f>= P        |
   |               | to pick >  fit | E: F > P > f     | F: P >= F > f        |
   \---------------+----------------+------------------+----------------------/
       
   -}
  context "2x2x3 (2 slices of 6)" do
     let box = Dimension 10 10 10
         shelf = Dimension 20 25 35
         orules = parseOrientationRule [] "9x9x9|"
         
         best toFit toPick = bestFitFor True
                                        (shelf, shelf)
                                        mempty 
                                        box
                                        orules
                                        toFit toPick

     it "fits  12/6" do  
       best 10 10 `shouldContain` [("how", "2x2x3") 
                                  ,("fit", " 12")
       --                                   ^ there is a space there
                                  ,("pickable", "6")
                                  ]
       
     context "required" do
       context "A: fit >= to Fit > to Pick" do
             it "full shelf 12/12/12" do
               best 24 10 `shouldContain` [("rvolmin100", "69") -- 20x20x30/20/25/35 = 20/25*30/35 = 0.6857
                                          ,("required", printDim $ Dimension 20 20 30)
                                          ,("refficiency100", "99")
                                          ]
             it "half shelf 12/6/6" do
               best 6 6 `shouldContain` [("rvolmin100", "34") -- 50% of 69
                                         ,("refficiency100", "99")
                                         ]
             it "round up 11/12 " do
               best 5 5 `shouldContain` [("rvolmin100", "34") -- 12/24
                                          ,("required", printDim $ Dimension 10 20 30)
                                          ,("refficiency100", "83") -- 5/6
                                          ]
             it "uses vertical slice 2/6" do
               best 3 1 `shouldContain` [("rvolmin100", "23") -- 10/20 * 20/25 * 20/35
                                        ,("required", printDim $ Dimension 10 20 20)
                                        --                                    
                                        ,("refficiency100", "75") -- 3/4
                                        ,("leftover_mode", "right")
                                        ,("r2100", "46") -- wh = 20/25*20/35 = 0.457
                                        ]
             it "uses horizontal slice 1/6 " do
               best 1 1 `shouldContain` [("rvolmin100", "06") -- 10/20 * 10/25 * 10/35 = 0.057
                                        ,("required", printDim $ Dimension 10 10 10)
                                        --                                 ^^ use full width and minimum height
                                        ,("refficiency100", "99") -- 1
                                        ,("leftover_mode", "above")
                                        ,("r2100", "14" ) -- lh = 10/20 * 10/35 = 0.1429
                                        ]
             context "picking" do
                -- extra stock ratio = to pick / all slots (and behind) needed to make everynhing pickable
                it "picks 2 slices" do
                   --   1/1   0
                   --   1/2   0
                   --   1/2 1/1
                   best 6 4 `shouldContain` [("pefficiency100", "50") -- 6 / 12
                                            ,("extra_stock_ratio", "0.50") --   tshow (6 / 12)) -- 6 to pick / 12 available
                                            ,("shelves_needed", "0.50")
                                            ,("picking_shelves", "0.67")
                                            ,("prequired", printDim $ Dimension 20 20 30)
                                            ,("debug_pbulk", "12")
                                            ,("debug_inFront", "(6,6)")
                                            ]
                it "picks 1 slice" do
                   --   1/1   0
                   --   1/2   0
                   --   1/2   0
                   best 5 3 `shouldContain` [("pefficiency100", "83") -- 5/ 6
                                            ,("extra_stock_ratio", "0.83") -- 5/6
                                            ,("pickable", "6")
                                            ,("prequired", printDim $ Dimension 10 20 30)
                                            ,("debug_pbulk", "6")
                                            ]
                it "picks 1/3 slice" do
                   --   0/0   0
                   --   1/1   0
                   --   1/2   0
                   best 3 2 `shouldContain` [("pefficiency100", "75") --  (3 / 4)) 
                                            ,("extra_stock_ratio", "0.75") --  (3/4))
                                            ,("prequired", printDim $ Dimension 10 20 20)
                                            ]
