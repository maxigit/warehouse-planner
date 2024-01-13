module WarehousePlanner.Brick.Repl
( module WarehousePlanner.Repl
, display
)
where

import ClassyPrelude
import WarehousePlanner.Repl
import WarehousePlanner.Brick
import Brick.Main


display :: IO ()
display  = do
  widget <- exec renderWarehouse
  simpleMain @() widget

