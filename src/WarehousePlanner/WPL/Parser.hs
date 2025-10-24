{-# LANGUAGE OverloadedStrings #-}
module WarehousePlanner.WPL.Parser
(
wplParser
, o1, o2, o3
)
where 

import ClassyPrelude hiding(some, many, try, (<|))
import WarehousePlanner.Selector (MParser, parseSelector, parseShelfSelector, parseBoxSelectorWithDef, splitOnNonEscaped)
import WarehousePlanner.WPL.Types
import Text.Megaparsec as P hiding((<?>))
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug qualified as P
import Text.Megaparsec.Char
import Data.Char
import Data.List.NonEmpty(nonEmpty, NonEmpty(..), some1)
import WarehousePlanner.Type
import Data.Foldable qualified as F
import Control.Monad(fail)
import WarehousePlanner.Base
import Data.Monoid (Last(..))
import Data.Map qualified as Map
import WarehousePlanner.Expr
import WarehousePlanner.ShelfOp(AbsRel(..))
import Data.Text (splitOn)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

-- import WarehousePlanner.WPL.PrettyPrint2


-- dbg _ = id
dbg :: Show a => String -> MParser a -> MParser a
-- dbg = P.dbg
dbg _ = id

-- avoid redundant import
_dbg :: Show a => String -> MParser a -> MParser a
_dbg = P.dbg
(<?>):: Show a => MParser a -> String -> MParser a
p <?> lbl = dbg lbl $ label lbl p
dlabel lbl p = p <?> lbl

wplParser :: MParser [Statement]
-- wplParser = sepBy p (lexeme ",") <* spaces  <* eof
wplParser = (optional "version:2" *> spaces) *> many p <* spaces  <* eof

class Parsable a where
   p :: MParser a

-- * Whitespaces & Co
-- starting with -- but we need a space so that --| is not a comment
lineComment :: MParser ()
lineComment = L.skipLineComment "---"
            <|> L.skipLineComment "-- "
            <|> try (void "--" <> lookAhead (void eol <|> eof)) -- finishinig a line


blockComment :: MParser ()
blockComment = L.skipBlockComment "{-" "-}"

-- | Spaces with new lines
spaces :: MParser ()
spaces = L.space space1
                 lineComment
                 blockComment

lexeme :: MParser a -> MParser a
lexeme = L.lexeme spaces
lexeme1 p = do
   r <- p
   lookAhead $ asum [space1, lineComment, blockComment, eof, void "}", void ")", void "]" ]
   spaces
   return r
   
{- rST::wpl-structure

Comment
-------

WPL accepts two types of comments :

- single line comment  starts with ``--``
- multiline comment between ``{-`` and ``-}``


Commands and selectors
----------------------

Commands starts with a lowercase and box selector with an uppercase.

Statement chain
---------------

By default, a statement is chained with the previous one, ie use output :ref:`context` of the previous statement unless the statement starts a new line.
In that case the statement will start a new chain. Statements between ``(...,...)`` are executed independently.

Example

::

   A B
   C

or

::

  A
    B
  C

is equilavent to 

::

   (A B , C)

Pass through
------------

The resulting context of a statment can be ignored with ``;`` beforehand.

Example 

::

   A (; B) C

Both ``B`` and ``C`` will be evaluated with the context output of ``A``.


::

   A -+-> B
      |
      +-> C
     
Whereas ``A B C`` is evaluated like

::

   A --> B --> C


Passthroughs are mainly used in case expression to ignore the result of the action.


Example

::

    [ A ; do_something
    | do_something_else
    ]


Apply something to ``A`` and something to everything but ``A``.
Without the ``;``, something else would  be applied to the left over of ``A do_something`` which can be different from just ``A``.

Block argument
--------------

Some commands require another statement as argument. This is the case for example of the command ``shelf:full``.
This command set the minimum dimensions of the selected shelves to the maximum ones for the duration of the next statement.
In order to avoid chaining ambiguity the next statement needs to be enclosed between ``{...}``.

Example

::

  shelf:full {A} B
  
executes ``A`` with full shelves and then execute B. Without the braces ``shelf:full A B`` could by interpreted as either ``shelf:full {A} B`` or ``shelf:full {A B}``.


If there is no ambiguity the ``&`` shorthand can be used, that is ``shelf:full {A}`` can be written as ``shelf:full & A``.

Property argument  
-------------------

Some commands accepts optional arguments. The general syntax is ``command name:value name2:value2 ...``. Most properties have an alias (usually the first letter) and if only argument exists the name of the argument doesn't need to be specified. If the command also accept a blok argument the property arguments need to be written before.


Example

::

  trace:count message:"box count" 
  trace:count msg:"box count" 
  trace:count m:"box count" 
  trace:count :"box count" 
  
  fill pos:$[old_position] { A }


are equivalent.

Control flow
------------


Conditional execution
`````````````````````

:ref:`Conditions  <condition>` can be used to do classic if then else. However the else is not optional, use ``when`` or ``unless`` instead.

IF THEN ELSE

Executes ``stmt1`` if ``cond`` is true, ``stmt2`` otherwise.
::

   if cond then  { stmt1 }  else { stmt2 }
   
WHEN

Executes ``stmt`` if the ``cond`` is true.

::

    when cond { stmt }
    
    
UNLESS
   
Executes ``stmt`` if the ``cond`` is false.

::

    unless cond { stmt }

Cases
`````
Cases exists in two flavours, box cases and shelf cases. Cases executes each statements with the left over :ref:`context` (either boxwise or shelfwise).
A case is as list of statement between ``[..]`` or ``/[..]`` separated by comma.

Example

::

    [ A -- select A
    , to> S1 -- move left over (no A) to S
    , to> S2 -- move left over (not fitting in S) to S2
    ]
    
Foreach block
`````````````

Do blocks executes a statement foreach boxes, shelves or contexts.


FOREACH:SHELF

Executes ``stmt`` for each shelves in ``shelves`` selector.

::

  foreach:shelf shelves { stmt }
  
FOREACH:BOX

Executes ``stmt`` for each boxes in ``boxes`` selector.

::

  foreach:box boxes { stmt }

FOREACH:DO


Executes ``stmt`` foreach context in in ``contexts``.

:: 

    foreach:do { smt } ( contexts )  


::rST -}
instance Parsable Statement where
         p = asum [  do
                      lexeme ";"
                      a <- lexeme p
                      return $ PassThrought a
                  , do -- if
                      lexeme1 "if"
                      cond <- p
                      then_ <- subStatement  <?> "then"
                      lexeme1 "else"
                      else_ <- subStatement <?> "else"
                      return $ If cond then_ (Just else_)
                  , do -- unless
                      lexeme1 "unless"
                      cond <- p
                      then_ <- subStatement  <?> "else"
                      return $ If (CondNot $ cond)  then_ Nothing
                  , do -- when
                      lexeme1 "when"
                      cond <- p
                      then_ <- subStatement  <?> "then"
                      return $ If cond then_ Nothing
                  , do
                     a <- lexeme atom
                     fm <- optional $ followUp
                     return case fm of 
                       Nothing -> a
                       Just f -> f a
                  ]
             where atom  = asum [ 
                           do
                             (o:os) <- between (lexeme "(") (lexeme ")") (sepBy1 (lexeme p) (lexeme $ ","))
                             return case os of
                                [] -> o
                                _ -> Ors $ o :| os
                          , withStatement "foreach:shelf" (lexeme1 "foreach:shelf" $> ForeachShelf)
                          , withStatement "foreach:box" do 
                                  lexeme1 "foreach:box" 
                                  boxes <- lexeme p
                                  return $ ForeachBox boxes
                          , do
                              lexeme "foreach:do"
                              st <- subStatement
                              (o:os) <- between (lexeme "(") (lexeme ")") (sepBy1 p (lexeme $ ","))
                              return $ ForeachDo st (o :| os)
                          , do
                             (c:cs) <- between (lexeme "/[") (lexeme "]") (sepBy1 p (lexeme $ "|"))
                             return $ ShelfCases $ fmap (\st -> ShelfCase st Nothing) (c:|cs)
                          , do
                             (c:cs) <- between (lexeme "[") (lexeme "]") (sepBy1 p (lexeme $ "|"))
                             return $ Cases $ fmap (\st -> Case st Nothing) (c:|cs)
                          , withStatement "trace:pretty" do
                              lexeme1 ("trace:pretty" <|> "t:p")
                              descM <- lexeme $ o1  ["m"]
                              return $ PrettyPrint (fromMaybe "T:P" descM)
                          , Action <$> p 
                          ]
                   followUp = asum [ do 
                                      pos <- getSourcePos
                                      if (unPos (sourceColumn pos) <= 1)
                                      then do
                                         spaces 
                                         return id
                                      else do
                                           b <- p
                                           return \a -> Then a b

                                   ]
                 
subStatement :: MParser Statement
subStatement = lexeme "{" *> lexeme p <* lexeme "}"
               <|> lexeme "&" *> lexeme p

withStatement :: String -> MParser (Statement -> a) -> MParser a
withStatement name parser = do
   constructor <- parser
   stmt <- subStatement <?> name
   return  $ constructor stmt

{- rST::wpl-commands

.. _commands:

Box manipulation
----------------

Boxes can moves around, tagged but also resized.

- ``to^ boxes:pmode:orules shelves``: moves current boxes to given shelves. Exit on top
- ``to> boxes:pmode:orules shelves``: moves current boxes to given shelves. Exit Left
- ``to  boxes:pmode:orules exit'shelves``: moves current boxes to given shelves. Accept a different exit mode for each shelf.
        Example

        ::

           to >A/[12]^B/[12]

.. todo:: pmode
.. todo:: orules


- ``swap debug:sticky selector``: Swap the current boxes to match the order given by the selector.
   The selector should only reorder the current selection. "Sticky" tags stays in place instead of moving with boxes.
   
   Example
   
   ::
   
     [ A | B | C ] swap [A | C | B ]

- ``fill pos:loc { boxes }``:  Move boxes using to the given position and location specified by ``pos`` and ``loc`` :ref:`property <evaluation>`. The default uses the ``#fill-shelf`` and ``#fill-shelf`` tags.

- ``tag #tag1#tag2``: tags the current boxes with tag1 and tag2
- ``tag #tag1#tag2 boxes? { stmt }``: execute stmt with tag set temporaly.
        Similar to ``(tag #tag1#tag2 , smt, tag #-tag1#-tag2)``.
        If the boxes parameter is given, only the boxes will be tag (without narrowing the context to those boxes)

- ``toggle #tag``: tag included and untag excluded.
- ``toggle #-tag``: untag included and tag excluded.
- ``box:resize [max|min|first] box: { stmt }``: resize temporalily all selected boxes with the given strategy. If a box selector is given  the given box will be used to calculate the new size.

    - ``bsize``
   
    Example
    
       ::
       
          box:resize first A { stmt }
          
       Resize with the dimension of the first encountered A.

       ::
       
          box:resize first { stmt }
          
       Resize with the dimension of the first selected box.

Box selectors
-------------

Narrows the current box selection.

- ``Selector``: select boxes. Start with an uppercase
- ``?selector``: select boxes. Start with an uppercase
- ``in:shelves``: select boxes in the current shelf selection.

Box Range
`````````

Select boxes upto, from etc the given selector.

- ``from selector``: after selector (included)
- ``upto selector``: before selector (include)
- ``after selector``: after selector (excluded)
- ``before selector``: before selector (excluded)

.. todo:: selector syntax

Shelf selectors
---------------

Narrows the current shelf selection.

- ``/selector``: select shelves.
- ``with:boxes``: select shelves containing in the current box selection.

Miscellaneous
-------------

- ``tam orules: tagAndMove``: tag and move current box selection as in :ref:`tag and moves section <moves-and-tag>`.
- ``delete``:  delete current box selection.

    .. danger::
       Deleting boxes doesn't remove them from the parents and current context.
       Chaining action after delete might cause crashes.

Context parameters
------------------

Miscellaneous "global" parameters can be set per context. When a parameter is set, it applies to that effect and the context and its children only (that is, every statements within the chain). The general syntax for setting global parameter is ``parameter=value``


- ``pmode=mode``: set the default default :ref:`partition mode <partition-mode>`.
    - ``p=mode``
    
- ``orules=rules selector?``: set the :ref:`orientation <orientation-rules>` for everything or the given selector.
- ``orules+=rules selector?``: add a rule  to the current orientation rules. The last rule has priority over the previous one.

.. _empty-check:

Empty checks
````````````

WHP can check if the result of a selector is empty or not. This can be used to make sure there is no typo in the selector or that the source boxes and target shelves exists.
When enabled, the WHP will stop and display a message if the condition is met.

Checks on the result of an action (like checking if all boxes have been moved successfully) can be done using :ref:`assertions <assertions>`.

- ``empty:boxes=no``: stops if box selector is empty.
- ``empty:boxes=yes``: allows empty boxes
- ``empty:shelves=no``: stops if shelf selector is empty.
- ``empty:shelves=yes``: allows empty shelves


Traces and Assertions
---------------------

Trace
`````

Trace can be logged to stdout. If a message argument is provided it would be used as the title.

- ``trace:count message:?``: log the number of selected boxes and shelves.

  - ``t:c``
  
- ``trace:boxes message:prop:``: log all selected boxes. If prop is given it will be expanded as :ref:`property <evaluation>` for each boxes.

  - ``trace:box``
  - ``t:b``

  Example
  
     ::
     
         trace:boxes prop:"boxname = ${boxname} $[dead?]"


- ``trace:shelf message:``: log all selected shelves.

  - ``t:s``
- ``trace:orientation message:``: log current orientation rules.

  - ``t:o``

.. _assertions:

Assertions
``````````

Assertions raise an error if the condition is not true. A message can be passed to help localizing which assertion failed. Unlike :ref:`empty checks <empty-check>` assertion are only checking the current context.

- ``assert:noboxes message:``: asserts that there is no selected boxes

  - ``a:nob``

- ``assert:boxes message:``: asserts that some boxes are selected.

  - ``a:b``

- ``assert:noshelves message:`` asserts that no shelves are selected
  
  - ``a:nos:``

- ``assert:shelves message:`` asserts that some shelves are selected.

  - ``a:s``

Shelf Manipulation
------------------

Shelves can tagged, resized or even split in grid. Shelf command accepting a block argument only perform the modification during the life span of the block.

- ``shelf:tag #tag``: tag the current shelves.
 
  - ``shelves:tag #tag``
  - ``stag #tag``

- ``shelf:full { stmt }``:  Set temporarily the current shelves minimum dimensions to their respective maximum dimensions.

- ``shelf:resize shelfves boxes:length:width:height { stmt }``: Resize the current shelves temporarily using dimension formula similar  to :ref:`shelf split <shelf-split>`. If a box selector is given, the first found box would be use ath "the box" in formula. Unlike ``shelf:split``, boxes out of the shelf are not removed.
  
     ::
     
         shelf:resize * height:{50%}  {stmt}

     Limit the current shelves to 50% of their respective height.

     ::
     
         shelf:resize * height:{=}*2 box:A  {stmt}

     Limit the current height to fit to boxes with an ``=`` orientation.

- ``shelf:split boxes:length:width:height { stmt }``: Split the current shelves temporarily using dimension formula similar  to :ref:`shelf split <shelf-split>`. If a box selector is given, the first found box would be use ath "the box" in formula. Multiple formula are separated with a ``:``.
  Example
  
     ::
     
         shelf:resize * height:{50%}  {stmt}

     Split selected shelves in 2 sub shelves of equal height.

     ::
     
         shelf:resize * height:{30%}:{30%}  {stmt}

     Split selected shelves in 3 sub shelves of different height (30% , 30% and 40%).

::rST -}
instance Parsable Command where
  p = dlabel "command" $ asum
        [ do -- Move
            exitMode <- (lexeme1 "to^" $> ExitOnTop) <|> (lexeme1 "to>" $> ExitLeft) 
            (sourceM, pmode, orules) <- o3 boxesK pmodeK oRulesK
            shelf <-  p
            return $ Move sourceM pmode (fromMaybe [] orules) $ singleton (exitMode, shelf)
        , do -- Move, the exitMode is after the options
            lexeme "to " -- avoid collision with toggle
            (sourceM, pmode, orules) <- o3 boxesK pmodeK oRulesK
            exit'shelves <- some1 $ liftA2 (,) (lexeme p) p
            return $ Move sourceM pmode (fromMaybe [] orules) exit'shelves
        , do -- Tag
           lexeme "tag"
           "#"
           tags <- lexeme1 $ str -- takeWhileP (Just "tags") (not . isSpace)
           let tagOps = parseTagOperations tags
           boxesM <- o1 boxesK
           case boxesM of
              Just boxes -> do
                         sub <- subStatement
                         return $ TagFor boxes tagOps sub
              Nothing -> do
                      subM <- optional $ subStatement <?> "sub"
                      case subM of
                           Nothing -> return $ Tag tagOps
                           Just sub -> do -- temporary tags
                                    return $ TagFor (CSelector selectAllBoxes) tagOps sub
        , do -- Toggle
           lexeme "toggle"
           "#"
           tagOps <- lexeme1 $ str -- takeWhileP (Just "tags") (not . isSpace)
           return $ ToggleTags (parseTagOperations tagOps)
        , do -- TagShelves
           lexeme ("shelf:tag"  <|> "shelves:tag" <|> "stag")
           "#"
           tagOps <- lexeme $ str -- takeWhileP (Just "tags") (not . isSpace)
           return $ TagShelves (parseTagOperations tagOps)
        , lexeme "with:boxes" $> SelectShelves CCrossSelection
        , do -- SelectShelves
           lexeme "/"
           sel <- p
           return $ SelectShelves sel
        , do -- SelectBoxes
           sel <- p
           return $ SelectBoxes sel
        , do -- Box ranges
             boundary <- asum $ [ lexeme1 "from" $> From
                                , lexeme1 "upto" $> Upto
                                , lexeme1 "before" $> Before
                                , lexeme1 "after" $> After
                                ]
             selector <- label "boundary selector" p
             return $ SelectBoxRanges boundary selector
        , do -- Tam
           lexeme1 "tam"
           orules <- o1 oRulesK
           tagLoc <- lexeme1 $ str <?> "loc#tag" --  takeWhile1P (Just "loc#tag") (not . isSpace)
           return $ TagAndMove tagLoc (fromMaybe [] orules)
        , lexeme1 "delete" >> return Delete
        , do
            lexeme ("pmode=" <|> "p=")
            pmode <- p
            return $ SetPartitionMode pmode
        , do
            cons <- lexeme $ asum [ SetOrientationStrategies <$ ("orules=" <|> "o=") 
                                  , AddOrientationStrategies <$ ("orules+=" <|> "o+=")
                                  ]
            os <- p
            selM <- o1 forK  
            return $ cons selM os

        , do -- TraceCount
            (lexeme1 $ "trace:count" <|> "t:c")
            descM <- o1  msgK
            return $ TraceCount (fromMaybe "T:C" descM)
        , do -- TraceBoxes
           lexeme1 $ asum ["trace:boxes", "trace:box", "t:b"]
           (descM, propM) <- o2 msgK propK
           return $ TraceBoxes (fromMaybe "T:B" descM) propM
        , do -- TraceShelves
            (lexeme1 $ "trace:shelves" <|> "t:s")
            descM <- o1  msgK
            return $ TraceShelves (fromMaybe "T:S" descM)
        , do -- TraceOrientations
            (lexeme1 $ "trace:orientation" <|> "t:o")
            descM <- o1  msgK
            return $ TraceOrientations (fromMaybe "T:O" descM)
        , do -- Set NoEmp
           lexeme1 $ "empty:boxes=yes"
           return $ SetNoEmptyBoxes False
        , do -- Set NoEmp
           lexeme1 $ "empty:boxes=no"
           return $ SetNoEmptyBoxes True
        , do
          lexeme1 "empty:shelves=no"
          return $ SetNoEmptyShelves True
        , do
            lexeme1 "empty:shelves=yes"
            return $ SetNoEmptyShelves False
        , do -- assert
          b <- (lexeme ("assert:noboxes" <|> "a:nob")  $> True)
               <|> (lexeme ("assert:boxes"  <|> "a:b") $> False)  
          descM <- o1 msgK
          return $ AssertBoxes b (fromMaybe (if b then "A:Boxes" else "A:Boxes") descM)
        , do -- assert
          b <- (lexeme ("assert:noshelves" <|> "a:nos")  $> True)
               <|> (lexeme ("assert:shelves"  <|> "a:s") $> False)  
          descM <- o1 msgK
          return $ AssertShelves b (fromMaybe (if b then "A:Shelves" else "A:Shelves") descM)
        , withStatement "shelf:full" do
                        lexeme1 $ "shelf:full"
                        sel <- p
                        return $ ResizeShelfFull sel
        , withStatement "shelf:resize" do
                        lexeme1 $ "shelf:resize"
                        sel <- p
                        boxM <- o1 boxesK
                        (lsm, wsm, hsm) <- o3 ["l", "length"] ["w", "width"] ["h", "height"]
                        let defRef = parseExpr "{}"
                        let [ls, ws, hm] = map (fromMaybe defRef ) [lsm, wsm, hsm]
                        return $ ResizeShelf sel boxM ls ws hm
        , withStatement "box:resize" do
                        "bsize:"
                        mode <- asum [ lexeme1 "max"  $> MaxDimension
                                     , lexeme1 "min" $> MinDimension
                                     , lexeme1 "first" $> FirstDimension
                                     ]
                        selM <- o1 boxesK
                        return $ ResizeBox mode (fromMaybe (CSelector selectAllBoxes) selM)
        , withStatement "shelf:split" do
                        lexeme1 $ "shelf:split"
                        sel <- p
                        boxM <- o1 boxesK
                        (lsm, wsm, hsm) <- o3 ["l", "length"] ["w", "width"] ["h", "height"]
                        let [ls, ws, hm] = map (fromMaybe [] ) [lsm, wsm, hsm]
                        return $ SplitShelf sel boxM ls ws hm
        , do
                        lexeme1 "swap"
                        (debugPrefixM, stickym) <- o2 ["debug", "d"] ["sticky", "s"]
                        selector <- p
                        let stickies = maybe [] (splitOnNonEscaped "#") stickym
                        return $ SwapBoxes selector debugPrefixM stickies
        , withStatement "fill" do
            lexeme1 "fill"
            (posM, locM) <- lexeme $ o2 ["pos", "p"]  ["loc", "l"]
            return $ FillShelves locM posM

        ]
        where msgK = [ "msg",  "message", "m"]
              boxesK =  [ "boxes", "box", "b"]
              pmodeK = [ "pmode" , "p"]
              oRulesK = [ "orules" , "o"]
              propK = [ "prop", "property", "pr" ]
              -- shelvesK = ["shelf", "shelves", "s"]
              forK = ["for", "f"]
              
-- * Selector
instance Parsable BoxSelector where
   p = dlabel "box selector" $ do
                guardLower
                t <- lexeme1 (takeWhile1P (Just "selector") isSelector)
                return $ parseBoxSelectorWithDef False t
    where guardLower :: MParser ()
          guardLower = label "Escape lower case with ?" $ void $ (char '?') <|> lookAhead ( asum [ upperChar, char '^', char '#', char '!', char '*'])
          --                                                                                                  ^^^^^^^^  ^^^^^^^        ^^^
          --                                                                                                     |          |           |
          --                                                                                                     |          |           +-- negation
          --                                                                                                     |          +-------------- tag selection
          --                                                                                                     +------------------------- number limit
         

instance Parsable ShelfSelector where
  p = dlabel "shelf selector" $ asum
            [ do
                "?"
                t <- getText
                return $ parseShelfSelector t
            ,
              do
                t <- getText
                return $ ShelfSelector SelectAnything (parseSelector t)
            ]
      where isShelfSelector c = isSelector c && (c `notElem` ("<^>" :: String)) 
            --                                                 ^^^
            --                                                 ||+--- exitmode left
            --                                                 |+---- exitmode top
            --                                                 +----- start of numeric range
            --                                                        which end with >
            getText = do
                      ts <- lexeme $ some1 (range <|> takeWhile1P (Just "selector") isShelfSelector)
                      return $ sconcat ts
            range = do
                     char '<'
                     r <- takeWhile1P (Just "range") (/='>')
                     char '>'
                     return $ "<" <> r <> ">"


       
instance Parsable (Selector s) where
   p = do 
        t <- lexeme $ takeWhile1P (Just "selector") isSelector
        return $ parseSelector t
instance Parsable (CSelector BoxSelector) where
  p  =  label "cbox" $ asum
    [  lexeme1 "in:shelves" >> return CCrossSelection
    ,  lexeme1 "in" >> do 
                          shelves <- p
                          return  $ CSelector (selectAllBoxes { shelfSelectors = shelves} )
    , cselector
    ]


instance Parsable (CSelector ShelfSelector) where
  p = label "cshelf" $ asum
     [ lexeme1 "with:boxes" >> return CCrossSelection
     , lexeme1 "with" >> do 
                            BoxSelector b s _  <- p
                            return $ CSelector (ShelfSelector b s)
     , cselector
     ]
isSelector :: Char -> Bool
isSelector c = not $ isSpace c || c == ')'


cselector :: Parsable s => MParser (CSelector s)
cselector = try $  do
        gs <- many go
        selectorm <- case gs of
          [] -> Just <$> sel
          _ -> lexeme $ optional sel
        case gs <> toList selectorm of 
           [] -> fail "some returning []"
           [one] -> return one
           cs -> return $ F.foldr1 CSelectorAnd cs
  where go = asum [swapContext, root, parent, useContext, cstatement] where
        swapContext = do
            (string "-~")
            return SwapContext
        sel = CSelector <$> p
        parent = (char '~') >> return Parent
        root = (string ".~") >> return Root
        useContext = "xsel" >> return CCrossSelection
        -- ^^^^^^ not to be used by human. here for quickcheck : to be able to parse
        -- pretty print.
        cstatement = lexeme "(" *> (CStatement <$> lexeme p <?> "cstatement") <* ")"
                     --                                           ^^^^^
                     --                                            |
                     --  No lexeme in here. Chain of selectors ----+
                     --  can not be separated by space

instance Parsable [OrientationStrategy] where
  p = ( lexeme1 "<empty>" >> return []) <|> do
    rule <- lexeme $ takeWhile1P (Just "orientation rules") (not . isSpace)
    let rules = splitOn "," rule
    mconcat <$> mapM go rules
    where go rule = case parseOrientationRule [tiltedForward, tiltedFR] rule of
                         [] | not (null rule) -> fail "not an rule"
                         rules -> return rules
          
instance Parsable PartitionMode where
  p = partitionModeParser

instance Parsable ExitMode where
  p = ("^" $> ExitOnTop) <|> (">" $> ExitLeft )
-- * Option utilites

   
instance Parsable Text
  where p = str
  
instance Parsable (Expr Text) where
   p = lexeme exprParser
   
instance Parsable (AbsRel (Expr Text)) where
   p = asum [ do
              "!" 
              e <- p
              return $ Abs e
            , Rel <$> p
            ]
instance Parsable [Expr Text] where
  p = lexeme $ exprParser `P.sepBy1` lexeme ":"

instance Parsable [AbsRel(Expr Text)] where
  p = lexeme $ p `P.sepBy1` lexeme ":"
str :: MParser Text
str = asum [ pack <$> (char '"' >> manyTill L.charLiteral (char '"' ))
           , do "{" ; fail "string are not allowed to start with {"
           , takeWhileP (Just "string") (\c -> not $ isSpace c || c == ')')
           ]
    
o :: Show a => Parsable a => [Text] -> MParser a
o [] = o [""]
o keys@(k:_) = do
   void $ asum [ string (key <> ":") | key <- toList keys ]
   p <?> unpack k
    
-- | Add "" to a list of keys
o0 :: Show a => Parsable a => [Text] -> MParser a
o0 keys = o (keys ++ [""])

combine2 :: Show a => Show b => MParser a -> MParser b -> MParser (Maybe a, Maybe b)
combine2 pa pb = do
     es <- some $ lexeme (((,Nothing) . Just)  <$> try pa <|> ((Nothing,) . Just) <$> pb )
     return case nonEmpty $ map (bimap Last Last) es of 
        Nothing -> (Nothing, Nothing)
        Just ne -> bimap (getLast) (getLast) $ sconcat ne
     -- aOrbM <- optional $ Left <$> pa <|> Right <$> pb
     -- case aOrbM of
     --    Nothing -> return (Nothing, Nothing)
     --    Just (Left a) -> do
     --              bm <- optional pb
     --              return (Just a, bm)
     --    Just (Right b) ->  do
     --              am <- optional pa
     --              return (am, Just b)


o1 :: Show a => Parsable a => [Text] -> MParser (Maybe a)
o1 = optional . o0

o2 :: Show a => Show b => Parsable a => Parsable b => [Text] -> [Text] -> MParser (Maybe a, Maybe b)
o2 a b = do
    checkForDuplicate [a, b ]
    fromMaybe (Nothing, Nothing) <$> ( optional $ combine2 (o0 a) (o b) )

o3 :: (Parsable a, Parsable b, Parsable c, Show a, Show b, Show c) => [Text] -> [Text] -> [Text] -> MParser (Maybe a, Maybe b, Maybe c)
o3 ka kb kc = do
    checkForDuplicate [ka, kb, kc]
    (abm, cm) <- fromMaybe (Nothing, Nothing) <$> ( optional $ combine2 (combine2 (o0 ka) (o kb)) (o kc) )
    let (am, bm) = fromMaybe (Nothing, Nothing) abm
    return (am, bm, cm)

-- | Check that all set of key don't have duplicate
-- raise a parsing in error if needed
checkForDuplicate :: [[Text]] -> MParser ()
checkForDuplicate keyss = let
    keysWithCount = Map.fromListWith (+) $ map (,1 :: Int) $ concat keyss
    in case Map.filter (>1) keysWithCount  of
        dupMap | null dupMap  -> return ()
        dupMap ->  let errors = [ ErrorFail $ "'" <> unpack k <> ": used for different options"
                                | k <- keys dupMap
                                ]
                   in P.registerFancyFailure $ setFromList errors


{- rST::wpl-condition
 
 .. _condition:

Conditions are evaluated within the current context and are true if the given selector returns at least one object.
The selector can be a box or a shelf (starting with ``/``). Both selectors needs to be  enclosed within ``(..)``

Classic logical operators (``!``, ``&&`` and ``||``) can be used to combine conditions.

Example

::

   ( A ) --  true if there is a A box
   ( A* *B) -- true if there is a starting with A and ending with B
   (/S) -- if there is a S shelf
   !( A ) && (/S) --  no A box and a shelf S 


::rST -} 

instance Parsable Condition where
  p = makeExprParser term table <?> "conditions" where
      term = between (lexeme "(") (lexeme ")") p <|> asum [ "/" *> do CondShelf <$> p
                                                          , CondBox <$> p
                                                          ]
      table = [ [ prefix "!" CondNot ]
              , [ binary (lexeme "&&") CondAnd ]
              , [ binary (lexeme "||") CondOr ]
              ]
      prefix symbol f = Prefix (f <$ symbol)
      binary op f = InfixL (f <$ op)

  -- p = asum [ "!" *> do CondNot <$> p 
  --          ]

