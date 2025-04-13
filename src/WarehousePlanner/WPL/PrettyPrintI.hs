module WarehousePlanner.WPL.PrettyPrintI
(
prettyI
)
where

import ClassyPrelude
import WarehousePlanner.WPL.Types
import WarehousePlanner.Type
import Data.List.NonEmpty (NonEmpty(..))
import WarehousePlanner.Selector (printBoxSelector, printShelfSelector)

prettyI :: Statement -> Text
prettyI = prettyStatement True ""


prettyStatement :: Bool -> Text -> Statement -> Text
prettyStatement raw indent stmt = 
    case stmt of
       Action command ->  i <> prettyCommand nextIndent command
       Ors (stmt :| []) -> prettyStatement True "" stmt
       Ors stmts -> withPrefix prettyStatement "(o)" stmts
       Cases stmts -> withPrefix prettyCase "" stmts
       Then a b -> withPrefix prettyStatement "&" (a :| [b])
       -- Then a b -> prettyStatement raw indent a <> "\n" <>  prettyStatement False nextIndent b
       PassThrought stmt -> prettyStatement raw indent stmt
       ForeachDo stmt stmts -> i <> "foreach:do " <> prettyStatement True (indent <> "         " ) stmt <> "\n" <> indent <>  withPrefix prettyStatement ("foreach:- ") stmts
       st -> i <> pack (show st)
    where nextIndent = addIndent indent
          withPrefix :: forall s . (Bool -> Text -> s -> Text) -> Text -> NonEmpty s -> Text
          withPrefix pretty prefix stmts = let indentWithPrefix =  indent <> replicate (length prefix + 1) ' ' 
                                           in unlines [ i <> prefix <> " " <> pretty True indentWithPrefix stmt
                                                      | (i, stmt) <- zip ("" : repeat indent) ( toList stmts)
                                                      ]
          i = if raw 
              then ""
              else indent
              
prettyCommand :: Text -> Command -> Text
prettyCommand indent command = 
    join case command of
       Move source pmode ors dest exitMode -> [ maybe "" (prettyCSelector "" prettyBoxSelector) source
                                              , case exitMode of
                                                 ExitLeft -> "to>"
                                                 ExitOnTop -> "to^"
                                              , maybe "" tshow pmode
                                              , case ors of
                                                  [] -> ""
                                                  _ -> "orules " <> mconcat (map tshow ors)
                                              , prettyCSelector (addIndent indent) prettyShelfSelector dest
                                              ]
       Tag tagOps -> "tag ": map tshow tagOps
       TagFor selector tagOps stmt -> "tag:for " 
                                    : prettyCSelector indent prettyBoxSelector selector
                                    : map tshow tagOps
                                    <> [prettyStatement True (addIndent indent)  stmt]
       SelectBoxes selector -> [ prettyCSelector indent prettyBoxSelector selector ]
       SelectShelves selector -> [ "/" , prettyCSelector indent prettyShelfSelector selector ]
       TraceBoxes desc propm -> [ "trace:boxes"
                                 , desc
                                 ]
                                 <>  case propm of
                                     Nothing -> []
                                     Just prop -> ["with", prop ]
       TraceShelves desc -> [ "trace:shelves"
                                 , desc
                                 ]
       TraceOrientations desc -> [ "trace:orules", desc ]
       SetPartitionMode mode -> ["place", tshow mode]
       SplitShelf selector bselectm l w h stmt -> [ "split"
                                             , prettyCSelector indent prettyShelfSelector selector
                                             ]
                                             <> case bselectm of
                                                     Nothing -> []
                                                     Just b ->  [ "for"
                                                                , prettyCSelector indent prettyBoxSelector b
                                                                ]
                                             <> map tshow [l, w, h]
                                             <> [prettyStatement True (addIndent indent) stmt]

       _ -> [ tshow command ]
    where join = unwords . filter (not . null)
    
addIndent :: Text -> Text
addIndent = ("     " <>)
    
prettyCase :: Bool -> Text -> Case -> Text
prettyCase raw indent (Case cas statementm) =
    case statementm of
       Nothing -> "|| " <> prettyStatement raw (indent <> "   ") cas
       Just stmt -> "|  " <> prettyStatement raw (indent) cas
                          <> "\n" <> indent <> "   "
                         <> prettyStatement True (indent <> "   ") (Ors (stmt :| []))
      

prettyBoxSelector = printBoxSelector
prettyShelfSelector = printShelfSelector


prettyCSelector :: Text -> (s -> Text) -> CSelector s -> Text
prettyCSelector indent pretty csel = 
     case csel of
        SwapContext -> "-~"
        Parent -> "~"
        Root -> ".~"
        CStatement statment ->  "( " <> prettyStatement False (addIndent indent) statment <> "\n" <> indent <> ")"
        CUseContext ->  "<useContext>"
        CSelectorAnd c1 c2 -> prettyCSelector indent pretty c1 <> prettyCSelector indent pretty c2
        CSelector s -> pretty s

