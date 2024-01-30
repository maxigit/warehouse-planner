module WarehousePlanner.Brick.Repl
( module WarehousePlanner.Repl
, display
)
where

import ClassyPrelude
import WarehousePlanner.Repl
import WarehousePlanner.Brick
import Brick.Main
import Brick.AttrMap (attrMap)
import Graphics.Vty.Attributes (defAttr)


display :: IO ()
display  = do
  widget <- exec renderWarehouse
  let attMap = attrMap defAttr generateLevelAttrs 
  defaultMain ((simpleApp @() widget) {appAttrMap = const attMap })  ()

