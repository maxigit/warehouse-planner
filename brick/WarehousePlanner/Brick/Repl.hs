module WarehousePlanner.Brick.Repl
( module WarehousePlanner.Repl
, display
)
where

import ClassyPrelude
import WarehousePlanner.Repl
import WarehousePlanner.Brick.App
import Control.Monad.State (get)


display :: IO ()
display  = do
  let wh = fmap Right $ exec get
  whMain ClassyPrelude.id "repl" wh


