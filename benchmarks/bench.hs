import ClassyPrelude
import Criterion.Main
import WarehousePlanner.Repl
import WarehousePlanner.Base
import WarehousePlanner.Exec
import Text.Printf

import Control.Monad.State (get, gets)



benchLoad paths = do
  initRepl "data"
  loads paths
  -- summary

main = defaultMain
  [ bgroup "load" [ bench ("with " <> show l <> "position") $ nfIO $ benchLoad ["Shelves/01-Shelves", "Stock/with-position-" <> show l]
                  | l <- [1000, 2000, 3000, 4300]
                  ]
  , bgroup "mocked" [ bench (printf "%s %dx%d" name nstyle nshelf) 
                          $ nfIO $ withMock @Int [1..nstyle] [1..nshelf] action
                    | nstyle <- [10, 20, 30,40, 50]
                    , nshelf <- [10, 20, 30,40, 50]
                    , (name :: String, action) <- [ ("empty", return 0)
                                        , ("move", do
                                              boxes_ <- gets boxes
                                              shelves_ <- gets shelves
                                              errors <-  moveBoxes ExitLeft  PRightOnly DontSortBoxes 
                                                        (toList boxes_)
                                                        (toList shelves_)
                                              return (length errors)
                                          )
                                        ]

                    ]
  , bgroup "full" [bench "with moves" $ nfIO $ benchLoad ["full-with-moves"]]
  ]
                 
                 
                 
-- | Generate a warehouse
-- with shelvesv repeated for each styles
mockWH :: Show n => [n] -> [n] -> WH () s
mockWH styles shelves = do
   let dim = Dimension 60 50 30
       bdim = Dimension 10 20 50
   forM_ styles \(tshow -> style) ->  do
        forM_ shelves \(tshow -> s) -> do
            let shelfName = style <> "-" <> s
            shelf <- newShelf shelfName Nothing dim dim 0 DefaultOrientation ColumnFirst
            forM_ [(content, i) 
                  | content <- [1..4]
                  , i <- [1..2]
                  ]
                  \(content, i) -> do
                   void $ newBox style (tshow content) bdim tiltedForward shelf allOrientations ("index-" <> tshow i : [])
                   
                   
withMock :: Show n => [n] -> [n] -> WH a RealWorld -> IO a
withMock styles shelves action = do
  initRepl ""
  exec (mockWH styles shelves >> action)
  
