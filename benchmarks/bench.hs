import ClassyPrelude
import Criterion.Main
import WarehousePlanner.Repl
import WarehousePlanner.Base
import WarehousePlanner.Move(moveBoxes)
import WarehousePlanner.Tiling(howManyWithDiagonal)
import WarehousePlanner.Exec
import WarehousePlanner.Report qualified as Report
import WarehousePlanner.Selector(splitOnNonEscaped)
import Text.Printf
import Data.Text(split)
import Constants

import Control.Monad.State (get, gets)



benchLoad paths = do
  initRepl "data"
  loads paths
  return ()
  
main = defaultMain
  [ bgroup "load" [ bench ("with " <> show l <> "position") $ nfIO $ benchLoad ["Shelves/01-Shelves", "Stock/with-position-" <> show l]
                  | l <- [1000, 2000, 3000, 4300]
                  ]
  , bgroup "summarize" [ bench ("with " <> show l <> "position") $ nfIO do
                              benchLoad ["Shelves/01-Shelves", "Stock/with-position-" <> show l ]
                              exec (Report.summary)
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
                                              result <-  moveBoxes PRightOnly DontSortBoxes 
                                                        (toList boxes_)
                                                        [(ExitLeft, toList shelves_)]
                                              return (maybe 0 length $ excluded result)
                                          )
                                        ]

                    ]
  , bgroup "full" [ bench "with moves" $ nfIO $ benchLoad ["full-with-moves"]
                  , bench "with slots" $ nfIO $ benchLoad ["pl-65"]
                  , bench "bare" $ nfIO $ benchLoad ["full"]
                  ]
  , bgroup "slow" [ bench "slow" $ nfIO $ benchLoad ["slow"]
                  ]
  , bgroup "check" [ bench "all" $ nfIO $ benchLoad ["full", "check" ]
                   ,  bench "slow" $ nfIO $ benchLoad ["slow", "check"]
                   ]
  , bgroup "wpl" [ bench base $ nfIO $ benchLoad [base, "wpl-move"]
                 | base <- ["full", "pl-65"]
                 ] 
  , bgroup "diagonal" ( (bench "66" $ nfIO do
                                           return . show $ howManyWithDiagonal (Dimension 66 1 36)
                                                                        (Dimension 66 1 36)
                                                                        (Dimension 10 1 13)
                        )
                      : [ bench (show i <> "x" <> show k) 
                              $ nfIO do
                                     let dim = Dimension 50 1 50
                                         in_ = Dimension i 1 k
                                     return . show $ howManyWithDiagonal dim dim in_
                        | (i,k) <- [(1, 1), (1,3) , (25,5)]
                        ]

                      )
  , bgroup "split" [ bench "lots" $ nfIO do
                                    let txt = withEscapeTags
                                    return $ splitOnNonEscaped "#" txt
                   , bench "not" $ nfIO do
                                    let txt = withEscapeTags
                                    return $ splitOnNonEscaped "!" txt
                   , bench "no escape" $ nfIO do
                                    let txt = withEscapeTags
                                    return $ splitOnNonEscaped ";" txt
                   , bench "Text.split" $ nfIO do
                                    let txt = withEscapeTags
                                    return $ split (=='#') txt
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
                   
                   
withMock :: Show n => [n] -> [n] -> WH a RealWorld -> IO a
withMock styles shelves action = do
  initRepl ""
  exec (mockWH styles shelves >> action)
  
-- | Test howManyWith Diagonal
