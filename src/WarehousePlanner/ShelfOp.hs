module WarehousePlanner.ShelfOp 
( splitShelf
, unSplitShelf
, ds
, splitTo
, generateGrid
, parseExpr
, parseRef
, newShelfWithFormula
, shelfDimension
, evalExpr
, dimFromRef
, bottomToFormula
, dimToFormula
, ShelfDimension(..)
, dimForSplit
, AbsRel(..)
, absRelsToRels
) where
import ClassyPrelude
import WarehousePlanner.Base
import WarehousePlanner.Selector
import WarehousePlanner.Affine
import Data.List (scanl')
import Data.List qualified as List
import Data.List.NonEmpty(nonEmpty)
import WarehousePlanner.Expr qualified as E
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Data.Char(ord,chr)
import Control.Monad.State hiding(fix,mapM_,foldM)

-- | Dimension info to construct a Shelf
data ShelfDimension = ShelfDimension
  { sMinD :: Dimension
  , sMaxD :: Dimension
  , sBottomOffset :: Double -- 
  , sUsedD :: Dimension
  }
  deriving (Show)

-- | Offset ("altitude") of the top of a shelf
sTopOffset :: ShelfDimension -> Double
sTopOffset s = dHeight (sMaxD s) + sBottomOffset s

sAvailableD :: (ShelfDimension -> Dimension) -> ShelfDimension -> Dimension
sAvailableD d s = d s <> invert ( sUsedD s)

shelfDimension :: Shelf s -> WH ShelfDimension s
shelfDimension shelf = ShelfDimension (minDim  shelf) (maxDim shelf) (bottomOffset  shelf) <$> maxUsedOffset shelf

-- | Split a shelf into 2, 4 or 8 subshelves
splitShelf :: Shelf s -> [Double] -> [Double] -> [Double] ->  WH [Shelf s] s
splitShelf shelf@Shelf{..} ls ws hs = do
  let splits = generateGrid minDim ls ws hs
      adjustMax last accessor gDim = 
        if last -- last elemen of a grid the max need to be increased
        then 
          accessor gDim + accessor maxDim - accessor minDim
        else
          accessor gDim
      go split@Split{..} = do
        let newMax = Dimension (adjustMax ll dLength gDim)
                               (adjustMax lw dWidth gDim)
                               (adjustMax lh dHeight gDim)
            tagOps = [("/" <> shelfName, SetTag)
                     , ("/l", SetValues [tshow $ 1 + il])
                     , ("/w", SetValues [tshow $ 1 + iw])
                     , ("/h", SetValues [tshow $ 1 + ih])
                     , (suffix, SetTag) -- include "/" so first shelf => /aaa
                     ]
            suffix = pack $ "/" <> map (\i -> chr $ i + 97) [ il, iw , ih ]
        if (il, iw, ih) == (0,0,0)
        then -- original shelf
          updateShelf (\s -> s {minDim =  gDim, maxDim = newMax}) shelf >>= updateShelfTags tagOps 
        else do -- new one
          -- create it
          let newMin = gDim

          new <- newShelf (shelfName <> suffix)
                          (Just $ intercalate "#" $ flattenTags shelfTag )
                          newMin
                          newMax
                          (dHeight gOffset)
                          shelfBoxOrientator
                          shelfFillingStrategy
                  >>= updateShelfTags tagOps
          -- steal all boxes which belongs to it
          shelfBoxes <- findBoxByShelf shelf
          forM shelfBoxes (\box -> do
            case dimInSplit split (boxOffset box ) of
              Nothing -> return ()
              Just offset -> do
                assignShelf (Just new) box
                updateBox (\b -> b { boxOffset = offset }) box
                return ()
            )
          return new

  mapM go splits

-- | Generates breaks  so that
--    [1 2 4] 10 generates
--    0123456789
--    1224444555
--    1 0 :: (length offset
--    2 1
--    4 3
--    3 7
splitTo :: [Double] -> Double -> [(Double,Double, Bool)] -- ^ interval width, offset last
splitTo [] maxX = [(maxX, 0, True)]
splitTo (xs) maxX = let
  (offsets,_) = span (<maxX) (scanl' (+) 0 xs) 
  in reverse $ case reverse $ zip3 xs offsets (repeat False) of
    (last@(x, offset,_): reversed) -> 
      let most = x+offset
      in if most >= maxX
          then (maxX -offset, offset, True) : reversed
          else -- add last element
            (maxX - most, most, True) : last : reversed
    _ -> error "List should not be empty"
  

data Split = Split 
  { gDim :: Dimension
  , gOffset :: Dimension
  , il, iw, ih :: Int -- indexes
  , ll, lw, lh :: Bool -- last 
  } deriving (Show)
generateGrid :: Dimension -> [Double] -> [Double] -> [Double] -> [Split]
generateGrid Dimension{..} ls ws hs =
  [ Split{..}
  | (il, (l,ol,ll)) <- zip [0..] $ splitTo ls dLength
  , (iw, (w,ow,lw)) <- zip [0..] $ splitTo ws dWidth
  , (ih, (h,oh,lh)) <- zip [0..] $ splitTo hs dHeight
  , let gDim = Dimension l w h
  , let gOffset = Dimension ol ow oh
  ]


ds :: [Dimension -> Double]
ds = [dLength, dWidth, dHeight]
-- | Check if a given dim belongs to a split 
-- and return the relative offset
dimInSplit :: Split -> Dimension -> Maybe Dimension
dimInSplit Split{..} dim = let
  splitAff = AffDimension gOffset $ gOffset <> gDim
  e = 2*1e-5
  box = AffDimension dim (dim <> Dimension e e e)
  in fmap (\a -> aBottomLeft a <> invert gOffset) $ affDimensionIntersection splitAff box



-- | Unsplit a shelf by combining all its "children<
-- to it
unSplitShelf :: Shelf s -> WH (Shelf s) s
unSplitShelf shelf = do
  children <- findShelfBySelector (parseSelector $ shelfName shelf ++ "/*") >>= mapM findShelf
  let oMaps = [offsetsFromIndex (minDim shelf) i f children
                             | (i, f) <- zip [1..] ds
                             ]
  -- traceShow("SHELF", shelf)
  -- traceShow("children", children)
  -- traceShowM ("OMAPS", oMaps)
  lwhS <- forM children $ \child -> do
    let [ol,ow, oh] = [findWithDefault 0 (childIndex child i) m 
                  | (i,m) <- zip [1..3] oMaps
                  ]
        offset = Dimension ol ow oh
    boxes <- findBoxByShelf child
    -- reassign each box the original shelf
    newBoxes <-  mapM (updateBox (\box -> box {boxOffset = boxOffset box <>  offset})) boxes
    mapM (assignShelf (Just shelf)) newBoxes
    deleteShelf $ shelfId child
    let Dimension ml mw mh = offset <> minDim child
    return (ml, mw, mh)
  -- compute new shelf size
  let  (mls, mws, mhs) = unzip3 lwhS
       [ldiff, wdiff, hdiff] = map (\f -> maximumEx [ f (maxDim s) - f (minDim s) 
                                                 | s <- shelf : children
                                                 ]
                                   )
                                   ds
       newMin = Dimension (maximumEx mls) (maximumEx mws) (maximumEx mhs)
       newMax = newMin <> Dimension ldiff wdiff hdiff
       tagOps = [ ("/" <> shelfName shelf, RemoveTag)
                , ("/l", RemoveTag)
                , ("/w", RemoveTag)
                , ("/h", RemoveTag)
                , ("/aaa", RemoveTag)
                ]
  updateShelf (\s -> s {minDim = newMin, maxDim = newMax }) shelf >>= updateShelfTags tagOps

-- | Extract the n index from a child nmae
-- index starts a 1
-- from exapmle childIndex 2 shelf/abc => b
childIndex :: Shelf s -> Int -> Char
childIndex = indexFromName . shelfName
indexFromName :: Text -> Int -> Char
indexFromName name z = let
  i = length name - 4 + z
  in fromMaybe 'a' $ index name i

  
  

-- | The naming scheme of all children
-- include a 3 letter index from a-z
-- We know that all children have been 
-- generated in a grid way or slices
-- within a slice (same index) all shelves should have the same
-- "thickness".
-- This function computes the offset for each slices given a direction
offsetsFromIndex :: Dimension -> Int -> (Dimension -> Double) -> [Shelf s] -> Map Char Double
offsetsFromIndex dim0 z f shelves = let
  widthMap :: Map Char Double
  widthMap = mapFromList [(childIndex s z, f (minDim s)) | s <- shelves]
  (indexes, widths) = unzip $ mapToList widthMap
  base = case indexes of
          'a':_ -> 0
          _ -> f dim0
  offsets = scanl' (+) base widths
  in mapFromList $ zip indexes offsets
  

data RefE = RefE Text (ShelfDimension -> Double)
type Expr = E.Expr RefE

parseExpr :: (ShelfDimension -> Double) -> Text -> Expr
parseExpr defaultAccessor "" =  E.ExtraE $ RefE "%" defaultAccessor
parseExpr defaultAccessor s =  fmap parse $ E.parseExpr s
   where parse t = case P.parse (parseRef defaultAccessor) (unpack t) t of
                     Left err -> error $ "Split shelf parameter invalid: "
                              <> show s
                              <> " "
                              <> show err
                     Right v -> v

parseRef :: (ShelfDimension -> Double) -> MParser RefE
parseRef accessor = do
  ref <- P.takeWhileP Nothing (/=':') -- P.many (P.noneOf ":}") --  (P.alphaNum <|> P.oneOf ".+-%_\\")
  acc <- P.option accessor $ P.char ':' *> parseAccessor
  return $ RefE ref acc

parseAccessor ::  MParser (ShelfDimension -> Double)
parseAccessor = P.choice $ map  (\(s ,a) -> P.try (P.string s) >> return a)
                $ concatMap pre
                [ (["length", "l"], dLength . sMinD)
                , (["width", "w"], dWidth   . sMinD)
                , (["height", "h"], dHeight . sMinD)
                , (["Length", "L"], dLength . sMaxD)
                , (["Width", "W"],  dWidth   . sMaxD)
                , (["Height", "H"], dHeight . sMaxD)
                , (["bottom", "b"], sBottomOffset)
                , (["top", "t"], sTopOffset)
                , (["usedLength", "ul"], dLength . sUsedD)
                , (["usedWidth", "uw"], dWidth   . sUsedD)
                , (["usedHeight", "uh"], dHeight . sUsedD)
                , (["availableLength", "al"], dLength . sAvailableD sMinD)
                , (["availableWidth", "aw"], dWidth   . sAvailableD sMinD)
                , (["availableheight", "ah"], dHeight . sAvailableD sMinD)
                , (["AvailableLength", "AL"], dLength . sAvailableD sMaxD)
                , (["AvailableWidth", "AW"], dWidth   . sAvailableD sMaxD)
                , (["AvailableHeight", "AH"], dHeight . sAvailableD sMaxD)
                ]
  where pre (names, a) = [(name, a) | name <- names ]


type RefToSDim s = Text -> WH ShelfDimension s

evalExpr :: RefToSDim s -> Expr -> WH Double s
evalExpr refToSDim expr = do
   exprDouble <- traverse (evalRef refToSDim) expr
   return $ E.evalExpr exprDouble

evalRef :: RefToSDim s -> RefE -> WH Double s
evalRef refToSDim (RefE ref accessor) = do
  fmap accessor (refToSDim ref)

dimFromRef :: Text -> Text -> WH ShelfDimension s
dimFromRef shelfName ref = do
  let refName = transformRef shelfName ref
  ids <- findShelfBySelector (Selector (NameMatches [MatchFull refName]) [])
  shelf <- case ids of
              [] -> error $ "Can't find shelf " ++ unpack refName ++ " when evaluating formula"
              [id_] ->  findShelf id_
              _ -> error $ "Find multiple shelves for " ++ unpack shelfName ++ "when evaluating formula."
  shelfDimension $ shelf

evalExprFromShelf :: Text -> Expr -> WH Double s
evalExprFromShelf shelfname = evalExpr (dimFromRef shelfname)

transformRef :: Text -> Text -> Text
transformRef a b = pack (transformRef' (unpack a) (unpack b))
transformRef'  :: String -> String -> String
transformRef'  "" ref = ref
transformRef' origin ('%': after) = take leftL origin ++ transformRef' (drop leftL origin) after
            where leftL = length origin - length after 
transformRef' origin ('*':after@(needle:_)) = 
    let (a, b) =  List.break (==needle) origin
                        in a <> transformRef' b after
transformRef' os ('\\':c:cs) = c:transformRef' os cs
-- symetry in the given range
-- ex [24] : 2 -> 4 3->3 4 -> 2
transformRef' (o:os) ('[':c:d:']':cs) = chr ni : transformRef' os cs where
  ci = ord(c)
  di = ord(d)
  oi = ord(o)
  ni = ci + di - oi
transformRef' (o:os) (c:cs) = case c of
  '_' -> o:transformRef' os cs
  '+' -> (succ o):transformRef' os cs
  '-' -> (pred o):transformRef' os cs
  _ -> c:transformRef' os cs
transformRef' _ [] = []

-- transformRef os cs = error $ "Non-exhaustive patterns catch "
--    ++ "\n\t[" ++ os ++ "]\n\t[" ++ cs  ++ "]"

dimToFormula :: (ShelfDimension -> Dimension) -> RefToSDim s -> (Text, Text, Text) -> WH Dimension s
dimToFormula sDim refToDim (ls, ws, hs) = do
  l <- eval (dLength . sDim) ls
  w <- eval (dWidth . sDim) ws
  h <- eval (dHeight . sDim) hs
  return $ Dimension l w h
  where -- eval :: (ShelfDimension -> Double) -> Text -> WH Double s
        eval accessor s = evalExpr refToDim (parseExpr accessor s)


bottomToFormula :: Text -> Text -> WH Double s
bottomToFormula name bs = evalExprFromShelf name  (parseExpr sBottomOffset bs)
-- | Create a new shelf using formula
newShelfWithFormula :: (WH Dimension s) -> (WH Dimension s) -> (WH Double s) -> BoxOrientator -> FillingStrategy -> Text -> Maybe Text ->  WH (Shelf s) s
newShelfWithFormula dimW dimW' bottomW boxo strategy name tags = do
  dim <- dimW
  dim' <- dimW'
  bottom <- bottomW
  newShelf name tags dim dim' bottom boxo strategy

-- | Resolves expr ref given a box and a shelf
-- Empty ref = shelf itself
-- orientation, the box according to the given orientation
-- content 
-- @TODOC
dimForSplit :: Maybe (Box s) -> Shelf s -> Text -> WH ShelfDimension s
dimForSplit boxm shelf ref = 
  case unpack ref of
    "" -> shelfDimension shelf
    "%" -> shelfDimension shelf
    "shelf" -> shelfDimension shelf
    "self" -> shelfDimension shelf
    "content" -> do
      dim <- maxUsedOffset shelf
      return $ toSDim dim
    "before" -> do
      dim <- minUsedOffset shelf
      return $ toSDim dim
    "box" | Just box <- boxm -> return $ toSDim (_boxDim box)
    "*" | Just box <- boxm -> do -- use box/shelf orientation
      getOrientations <- gets boxOrientations
      let (o:_) = map osOrientations (getOrientations box shelf) ++ [tiltedForward] -- ^ default
      return $ toSDim (rotate o (_boxDim box))
    [c] | Just box <- boxm ->
      return $ toSDim (rotate (readOrientation c) (_boxDim box))
    _ | Just boxSel <- stripPrefix "<" ref -> select LT boxSel 
      | Just boxSel <- stripPrefix "="  ref -> select EQ boxSel
      | Just boxSel <- stripPrefix ">"  ref -> select GT boxSel
    _ -> dimFromRef (shelfName shelf) ref
  where toSDim (Dimension l w h) = let
                dim = Dimension l w h -- (l - 1e-6) (w - 1e-6) (h - 1e-6)
                in ShelfDimension dim dim 0 dim
        select ord ref = do
                     boxes <- findBoxByShelf shelf
                     let  sel = case ref of 
                                    "" | Just box <- boxm  -> parseSelector (boxStyle box)
                                    "%" | Just box <- boxm  -> parseSelector (boxStyle box)
                                    --  no ref given, use given box style
                                    _ -> parseSelector ref
                          (selected, other) = partition (applySelector sel)  boxes
                          selectedA = map boxAffDimension selected
                          otherA = map boxAffDimension other
                     case nonEmpty selectedA of
                                     Nothing | ord == LT -> shelfDimension shelf
                                             | otherwise -> return $ ShelfDimension mempty mempty 0 mempty
                                     Just selectANN  -> let
                                         selectBound =  sconcat selectANN
                                         minL = dLength $ aBottomLeft selectBound
                                         maxL = dLength $ aTopRight selectBound
                                         leftOf aff = dLength (aTopRight aff) <= minL
                                         rightOf aff = dLength (aBottomLeft aff) >= maxL
                                         affE = case ord of
                                                    LT -> case nonEmpty $ filter leftOf otherA of
                                                           Nothing -> Left $ shelfDimension shelf
                                                           Just aff -> Right $ sconcat aff
                                                    EQ -> Right $ selectBound
                                                    GT -> case nonEmpty $ filter rightOf otherA of
                                                             Nothing -> Left $ return $ ShelfDimension mempty mempty 0 mempty
                                                             Just aff -> Right $ sconcat aff
                                         in case affE of 
                                                Left r -> r
                                                Right aff -> return $ ShelfDimension (aBottomLeft aff) (aTopRight aff) 0 mempty



data AbsRel a = Abs a | Rel a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Transform a list of absolute relative number
-- into a list of relative suitable for toSplit
-- @ Rel 100 Rel 50 Abs 160 => 100 50 10
-- @
absRelsToRels :: [AbsRel Double] -> [Double]
absRelsToRels rs = runDiff $ scanl' addR 0 rs where
   addR acc (Rel r) = acc + r
   addR acc (Abs a) = max a acc
   runDiff xs = [  d
                | (current, previous) <- zip (drop 1 xs) xs
                --                           ^^^^^^^^^
                --                               |
                --                               +-- 0 is dropped
                , let d = current - previous
                , d > 0
                ]
