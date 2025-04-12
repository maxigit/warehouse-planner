module WarehousePlanner.WPL.PrettyPrint2
(
pretty2
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

-- import WarehousePlanner.WPL.PrettyPrint qualified as P
pretty2 :: Statement -> Text
pretty2 = renderStrict
          . layoutSmart defaultLayoutOptions { layoutPageWidth = AvailablePerLine 80 0.4 }
          . pStatement

pStatement :: Statement -> Doc a
pStatement = \case 
   Action command -> pCommand command
   Ors (stmt :| []) -> pStatement stmt
   Ors (stmt :| stmts) -> group $ align $ encloseSep "( " (line <> ")") (flatAlt ", " " , ") $ map pStatement (stmt : stmts)
   Cases cases -> -- align $ brackets $ vcat $ map pCase $ toList cases
                 group $ encloseSep "[ " (line <> "]") (flatAlt  "| " " | ") $ map pCase (toList cases)
   -- Then a (Action action) -> pStatement a <+> align (pCommand action)
   Then a b -> group $ pStatement a <> nest 4 (line <> pStatement b)
   -- t@(Then _ _) -> let stmts = toList $ unThen t
   --                 -- in parens $ hsep $ punctuate softline (map pStatement stmts)
   --                 in align $ parens $ hsep $ (map pStatement stmts)
   PassThrought stmt -> ";" <+> pStatement stmt
   ForeachBox sel stmt -> "foreach:box" <+> align (vsep [ braces (pCSelector pBoxSelector sel)
                                                        , pStatement stmt
                                                        ]
                                                  )
   ForeachShelf stmt -> "foreach:shelf" <+> pStatement stmt
   ForeachDo stmt xs -> "foreach:do" <+> align (vsep [ braces (pStatement stmt)
                                                     , brackets (vsep $ map pStatement (toList xs))
                                                     ]
                                               )
                                               

   stmt -> error (show stmt)  "doda" -- $ P.pretty stmt
   
pBlock :: Statement -> Doc a
pBlock stmt = group $ align $ "{ " <> pStatement stmt <> line' <> "}"

pCase :: Case -> Doc a
pCase (Case com Nothing) = pStatement com
pCase (Case com (Just com2)) = group $ pStatement com  <> nest 4 (line <> ";" <+> pStatement com2)


pCommand :: Command -> Doc a
pCommand = \case
   SelectBoxes sel -> "?" <> pCSelector pBoxSelector sel 
   SelectShelves sel -> "/" <> pCSelector pShelfSelector sel 
   SelectBoxRanges boundary sel -> viaShow boundary <+> "?" <> pCSelector pBoxSelector sel
   Move _sourceM _pModeM _strats dest exitMode ->
        let move = case exitMode of
                        ExitLeft -> \r -> ">--->" <+> r
                        ExitOnTop -> \r -> ">===>" <+> "^" <> r
                     
        in move $ pCSelector pShelfSelector dest
   Tag tagOps -> "#" <> pretty (printTagOperations tagOps)
   TagFor sel tagOps stmt -> "#" <> braces (pCSelector pBoxSelector sel)
                                 <> pretty (printTagOperations tagOps)
                                 <> " $" <> nest 3 (pBlock stmt)
   Delete -> "delete"
   SetOrientationStrategies  selm os -> let pos =  hsep $ map (pretty . showOrientationStratety) os
                                        in braces $ case selm of
                                                     Just sel -> "/" <> pShelfSelector sel <+> pos
                                                     Nothing -> pos
   ResizeBox mode sel stmt -> let command = case mode of
                                              MaxDimension -> "bsize:max"
                                              MinDimension -> "bsize:min"
                                              FirstDimension -> "bzise:first"
                              in command <+> align (vsep [ pCSelector pBoxSelector sel
                                                         , pBlock stmt
                                                         ]
                                                   )
   ResizeShelf  sel l w h stmt -> let pE = viaShow
                                  in "shelf:resize" <+> align ( vsep  [ braces $ pCSelector pShelfSelector sel
                                                                     , pE l
                                                                     , pE w
                                                                     , pE h
                                                                     , pBlock  stmt
                                                                     ]
                                                             )
   ResizeShelfFull sel stmt -> "shelf:full" <+> align (vsep [ braces (pCSelector pShelfSelector sel)
                                                           , pBlock stmt
                                                           ]
                                                     )
   SplitShelf shelf boxm ls ws hs stmt -> let pshelf = pCSelector pShelfSelector shelf
                                              pexps = hsep . punctuate colon . map viaShow 
                                              pbox = case boxm of
                                                          Nothing -> mempty
                                                          Just box -> "for:" <> pCSelector pBoxSelector box
                                          in "split"  <+>  align (vsep [ braces (hsep $ [pshelf, pbox]
                                                                                         <> map pexps [ls, ws, hs]
                                                                                )
                                                                       , pBlock stmt
                                                                       ]
                                                                 )
   AssertBoxes b text -> "assert:" <> (if b then "boxes" else "noboxes") 
                                   <+> dquotes (pretty text)
   AssertShelves b text -> "assert:" <> (if b then "shelves" else "noshelves") 
                                   <+> dquotes (pretty text)

                                                    
                                              
   com -> error $ show com -- $ pretty $ P.pretty (Action com)

pBoxSelector = pretty . printBoxSelector
pShelfSelector = pretty . printShelfSelector
pCSelector :: (s -> Doc a) -> CSelector s -> Doc a
pCSelector pSel csel = 
     case csel of
        SwapContext -> "-~"
        Parent -> "~"
        Root -> ".~"
        CStatement statment -> parens $ pStatement statment
        CUseContext ->  "<useContext>"
        CSelectorAnd c1 c2 -> pCSelector pSel c1 <> pCSelector pSel c2
        CSelector s -> pSel s

