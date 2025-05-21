module WarehousePlanner.WPL.PrettyPrint
(
prettyWPL, prettyWPLs
)
where

import ClassyPrelude hiding(group)
import WarehousePlanner.WPL.Types
import WarehousePlanner.Type
import WarehousePlanner.Base(printTagOperations)
import Data.List.NonEmpty (NonEmpty(..))
import WarehousePlanner.Selector (printBoxSelector, printShelfSelector)
import Prettyprinter
import Prettyprinter.Render.Text
import Data.Map qualified as Map
import WarehousePlanner.Expr(Expr(..))
import Data.Char (isUpper)

-- import WarehousePlanner.WPL.PrettyPrint qualified as P
prettyWPL :: Statement -> Text
prettyWPL = renderStrict
          . layoutSmart defaultLayoutOptions { layoutPageWidth = AvailablePerLine 80 0.4 }
          . pStatement
          
prettyWPLs :: [Statement] -> Text
prettyWPLs = unlines . intersperse "" . map prettyWPL

pStatement :: Statement -> Doc a
pStatement = \case 
   Action command -> pCommand command
   Ors (stmt :| []) -> pStatement stmt
   Ors stmts@(_ :| _) -> orsEnclosed stmts
   -- Ors (stmt :| stmts) -> hcat $ punctuate (space<> "||" <> space) (map pStatement (stmt: stmts))
   Cases cases -> -- align $ brackets $ vcat $ map pCase $ toList cases
                 group $ align $ encloseSep "[ " (line <> "]") (flatAlt  "| " " | ") $ map pCase (toList cases)
   ShelfCases cases -> -- align $ brackets $ vcat $ map pCase $ toList cases
                 group $ align $ encloseSep "/[ " (line <> "]") (flatAlt  "| " " | ") $ map pShelfCase (toList cases)
   -- Then a (Action action) -> pStatement a <+> align (pCommand action)
   Then a b -> group $ pStatement a <> nest 4 (line <> pStatement b)

   --                 -- in parens $ hsep $ punctuate softline (map pStatement stmts)
   --                 in align $ parens $ hsep $ (map pStatement stmts)
   PassThrought stmt -> ";" <+> pStatement stmt
   ForeachBox sel stmt -> "foreach:box" <+> align (vsep [ (pCBox sel)
                                                        , pBlock stmt
                                                        ]
                                                  )
   ForeachShelf stmt -> "foreach:shelf" <+> align (pBlock stmt)
   ForeachDo stmt xs -> "foreach:do" <+> align ( pBlock stmt
                                                 <+> orsEnclosed xs
                                               )
   If (CondNot cond) stmt Nothing -> "unless" <+> align (vsep [ pCond cond
                                                              , pBlock stmt
                                                              ]
                                                        )
   If cond stmt Nothing -> "while" <+> align (vsep [ pCond cond
                                                     , pBlock stmt
                                                     ]
                                             )
                                                     
   If cond stmt (Just stmt2) -> "if" <+> align (vsep [ pCond cond
                                                        , pBlock stmt
                                                        , "else" <+> pBlock stmt2
                                                        ]
                                                  )
   PrettyPrint title stmt -> "trace:pretty" <> opt' "" viaShow (Just title)
                                      <+>pBlock stmt
                                               

   -- stmt -> error (show stmt)  "doda" -- $ P.pretty stmt
   
orsEnclosed :: NonEmpty Statement -> Doc a
orsEnclosed stmts =  group $ align $ encloseSep "( " (line <> ")") (flatAlt ", " " , ") $ map pStatement (toList stmts)
   
pBlock :: Statement -> Doc a
pBlock stmt = group $ align $ "{ " <> pStatement stmt <> line <> "}"

pCase :: Case -> Doc a
pCase (Case com Nothing) = pStatement com
pCase (Case com (Just com2)) = group $ pStatement com  <> nest 4 (line <> ";" <+> pStatement com2)

pShelfCase :: ShelfCase -> Doc a
pShelfCase (ShelfCase com Nothing) = pStatement com
pShelfCase (ShelfCase com (Just com2)) = group $ pStatement com  <> nest 4 (line <> ";" <+> pStatement com2)

pCommand :: Command -> Doc a
pCommand = \case
   -- old syntax
   Move sourceM pModeM strats ((exitMode, dest):|[]) ->
        hcat [  case exitMode of
                        ExitLeft -> "to>"
                        ExitOnTop -> "to^"
               , opt "boxes" pCBox sourceM
               , opt' "pmode" pPartitionMode pModeM
               -- ^ print even default value, has nothing don't mean default mode
               -- but previously set
               , optWithDefault "orules" null pRules (Just strats)
               , space
               , if isDefault dest
                 then "*"
                 else pCShelf dest
               ]
   Move sourceM pModeM strats dest ->
        hcat $ [ "to "
               , opt "boxes" pCBox sourceM
               , opt' "pmode" pPartitionMode pModeM
               -- ^ print even default value, has nothing don't mean default mode
               -- but previously set
               , optWithDefault "orules" null pRules (Just strats)
               , space
               ]
               <>
               [ case exitMode of
                    ExitOnTop -> "^"
                    ExitLeft -> ">"
                 <> if isDefault sel
                 then "*"
                 else pCShelf sel
               | (exitMode, sel) <- toList dest
               ]
                     
            
   Tag tagOps -> "tag#" <> pretty (printTagOperations tagOps)
   TagFor sel tagOps stmt -> "tag#" <> pretty (printTagOperations tagOps)
                                    <> opt "" pCBox (Just sel)
                                    <+> nest 3 (pBlock stmt)
   ToggleTags tagOps -> "toggle#" <> pretty (printTagOperations tagOps)
   TagShelves tagOps -> "shelf:tag#" <> pretty (printTagOperations tagOps)
   SelectBoxes sel -> pCBox sel 
   SelectBoxRanges boundary sel -> pretty (toLower $ tshow boundary) <+> pCBox sel
   SelectShelves sel -> let sep = case sel of 
                                    CSelector sel | "[" `isPrefixOf` printShelfSelector  sel -> " "
                                    _ -> ""
                            --- rewrite "/[" as "/ [" to not be parse as ShelfCase
                        in "/" <> sep <> pCShelf sel 
   TagAndMove op strats -> "tam" <> optWithDefault "orules" (==[]) pRules (Just strats)
                                 <+> pretty op
   Delete -> "delete"
   SetPartitionMode pmode -> "pmode=" <> pPartitionMode pmode
   SetOrientationStrategies  selm os -> "orules=" <> pRules os <> opt' "for" pShelfSelector selm
   AddOrientationStrategies  selm os -> "orules+=" <> pRules os <> opt "for" pShelfSelector selm
   TraceCount msg -> "trace:count" <+> optWithDefault "" (=="T:C") viaShow (Just msg)
   TraceBoxes msg propM -> hcat ["trace:boxes" , optWithDefault "" (=="T:B") viaShow (Just msg), opt' "property" viaShow propM ]
   TraceShelves msg -> "trace:shelves" <> optWithDefault "" (=="T:S") viaShow (Just msg)
   TraceOrientations msg -> "trace:orientation" <> optWithDefault "" (=="T:O") viaShow (Just msg)
   SetNoEmptyBoxes True -> "empty:boxes=no"
   SetNoEmptyBoxes False -> "empty:boxes=yes"
   SetNoEmptyShelves True -> "empty:shelves=no"
   SetNoEmptyShelves False -> "empty:shelves=yes"
   AssertBoxes b text -> "assert:" <> (if b then "noboxes" else "boxes") 
                                   <> opt' "" viaShow (Just text)
   AssertShelves b text -> "assert:" <> (if b then "noshelves" else "shelves") 
                                     <> opt' "" viaShow (Just text)
   ResizeBox mode sel stmt -> let command = case mode of
                                              MaxDimension -> "bsize:max"
                                              MinDimension -> "bsize:min"
                                              FirstDimension -> "bsize:first"
                              in command <> opt "boxes" pCBox (Just sel)
                                         <+> pBlock stmt
   ResizeShelf  sel boxm l w h stmt -> "shelf:resize" <+> (if isDefault sel
                                                      then "*"
                                                      else pCShelf sel
                                                      )
                                                <> opt "boxes" pCBox boxm
                                                <> opt "l" pExpr (Just l)
                                                <> opt "w" pExpr (Just w)
                                                <> opt "h" pExpr (Just h)
                                                <+> pBlock stmt
   ResizeShelfFull sel stmt -> "shelf:full" <+> align (vsep [ if isDefault sel 
                                                              then "*"
                                                              else pCShelf sel
                                                           , pBlock stmt
                                                           ]
                                                     )
   SplitShelf shelf boxm ls ws hs stmt -> "shelf:split" <+> (if isDefault shelf 
                                                             then "*"
                                                             else pCShelf shelf
                                                             )
                                                        <> opt "boxes" pCBox boxm
                                                        <> optWithDefault "l" null pExprs (Just ls)
                                                        <> optWithDefault "w" null pExprs (Just ws)
                                                        <> optWithDefault "h" null pExprs (Just hs)
                                                        <+> pBlock stmt
   SwapBoxes boxes debugPrefix stickies -> "swap" <> optWithDefault "debug" null pretty debugPrefix
                                                  <> optWithDefault "sticky" null (pretty . intercalate "#" ) (Just stickies)
                                                  <+> pCBox boxes
   FillShelves {..} -> "fill"  <> optWithDefault "loc" null pretty cLoc
                                                   <> optWithDefault "pos"  null pretty cPos
                                                   <+> pBlock cStmt
                                               

                                                    
                                              

pBoxSelector sel = let t =  printBoxSelector sel
                   in pretty case uncons t of
                        Just (c,_) | isUpper c || c == '^' || c == '#'  -> t
                        _ -> "?" <> t
                      
                     
pShelfSelector sel = (case sBoxSelectors sel of
                        SelectAnything -> mempty
                        _ -> "?"
                     ) <> pretty (printShelfSelector sel)
pCSelector :: (s -> Doc a) -> CSelector s -> Doc a
pCSelector pSel csel = 
     case csel of
        SwapContext -> "-~"
        Parent -> "~"
        Root -> ".~"
        CStatement statment -> parens $ pStatement statment
        CCrossSelection ->  "xsel"
        CSelectorAnd c1 c2 -> pCSelector pSel c1 <> pCSelector pSel c2
        CSelector s -> pSel s

pCBox :: CSelector BoxSelector -> Doc a
pCBox = pCSelector pBoxSelector
pCShelf :: CSelector ShelfSelector -> Doc a
pCShelf = pCSelector pShelfSelector
           
pRules [] = "<empty>"
pRules ostrats = let -- group stratetegies if possible regardless of diag and orientation
   groups = toList $ Map.fromListWith (<>) [ (o { osOrientations = up }, [o])
                                           -- ^ all tries need to have the same orientation 
                                           -- we are however not using it , (the key is discared once grouped
                                           | o <- reverse ostrats
                                           ]
   go [] = error "the unexpected happend!"
   go (group@(o:_)) = let orientations = map osOrientations group
                          diag = case group of 
                                  [_one] -> True
                                   -- ^^^^ don't show no diagonal indicator if there is only one orientations in the group.
                                   -- to be consistent with parsing which does need the ! for single orientations.
                                  _ -> all osUseDiagonal group 
                          mainO = o { osUseDiagonal = diag}
                      in pretty $ showOrientationStratety mainO <> mconcat ( map showOrientation' (drop 1 orientations)) 

   in hcat $ punctuate comma $ map go groups

-- pretty . intercalate "," $ map showOrientationStratety os
pPartitionMode :: PartitionMode -> Doc a
pPartitionMode = \case 
    PRightOnly -> "right"
    PAboveOnly -> "above"
    PBestEffort ->  "best"
    POverlap OLeft -> "overlap"
    POverlap ORight -> "oright"
    POverlap OAligned -> "oaligned"
    PSortedOverlap -> "sorted"
    PBehind -> "behind"
    PCorner c -> "corner" <> pretty c
    POr a b -> pPartitionMode a <> ","  <> pPartitionMode b

pExpr :: Expr Text -> Doc a
pExpr = \case
 AddE a b -> p a <> "+" <> p b
 SubE a b -> p a <> "-" <> p b
 MulE a b -> p a <> "*" <> p b
 DivE a b -> p a <> "/" <> p b
 MinE a b -> p a <> "&" <> p b
 MaxE a b -> p a <> "|" <> p b
 ValE v -> pretty v
 ExtraE ref -> "{" <> pretty ref <> "}"
 where p e = case e of 
              ValE _ -> pExpr e
              ExtraE _ -> pExpr e
              e -> "(" <> pExpr e <> ")"

pExprs  :: [Expr Text] -> Doc a
pExprs exs = hcat $ punctuate colon $ map pExpr exs 

opt :: (Eq a, HasDefault a) => Text -> (a -> Doc d) -> Maybe a -> Doc d

opt key p x = optWithDefault key isDefault p x

optWithDefault :: Eq a => Text -> (a -> Bool) -> ( a -> Doc d) -> Maybe a -> Doc d
optWithDefault key f p x = let x' = if maybe True f x
                                    then Nothing
                                    else x
                           in opt' key p x'
            

opt' ::  Text -> (a -> Doc d) -> Maybe a -> Doc d
opt' _ _ Nothing = mempty
opt' key p (Just x) = space <> pretty key <> ":" <> p x


----- Condition
pCond :: Condition -> Doc a
pCond = \case
      CondBox selector -> pCBox selector
      CondShelf selector -> "/" <> pCShelf selector
      CondNot cond -> "!" <> enclose cond 
      CondAnd c1 c2 -> enclose c1 <+> "&&" <+> enclose c2
      CondOr c1 c2 -> enclose c1 <+> "||" <+> enclose c2
      where enclose c = (case c of
               CondNot _ -> doEnclose
               CondBox _ -> id
               CondShelf _ -> id
               CondOr _ _ -> doEnclose
               CondAnd _ _ -> doEnclose
               ) $ pCond c
            doEnclose c = "(" <+> c <+> ")"
      

