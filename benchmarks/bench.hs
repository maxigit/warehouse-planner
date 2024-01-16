import ClassyPrelude
import Criterion.Main
import WarehousePlanner.Repl
import WarehousePlanner.Base
import WarehousePlanner.Exec
import Text.Printf



benchLoad paths = do
  initRepl "data"
  loads paths
  -- summary

main = defaultMain
  [ bgroup "load" [ bench ("with " <> show l <> "position") $ nfIO $ benchLoad ["Shelves/01-Shelves", "Stock/with-position-" <> show l]
                  | l <- [1000, 2000, 3000, 4300]
                  ]
  , bgroup "mocked" [ bench (printf "create mocked %dx%d" nstyle nshelf) $ nfIO $ withMock [1..nstyle] [1..nshelf] (return ())l
                    | nstyle <- [1, 10, 20, 50]
                    , nshelf <- [1, 10, 20, 50]
                    ]
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
                   
                   
withMock :: Show n => [n] -> [n] -> WH a s -> IO a
withMock styles shelves action = do
  execWH emptyWarehouse action


          
  
