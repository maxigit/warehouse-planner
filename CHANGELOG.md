# 2.1.0
## Common
- Allow $[tag]=$[tag2] in tag formula
- Add round coordinate (ol, ow, ol)
- Add tag-values report (--tag-values)
- Add "before" in shelf split formula
   find gap before (on the left) of the first box
- Add cornerN partition mode
    when there is a "stair" of available corner in as helf
    N specify which one to use 
    first corresponds to above
    last to "right"
- fix mixed boxes in mop report.
# WPL
- Add passthrough block (; ...)
- Add trace:shelves
- add trace:orules
- add trace:boxes!, show box Id instead of title
- add property to trace:boxes
    trace:boxes with "style=${style} $[ghosh?]"
- chain meta selector 
   `~~~` is parsed as ~ then ~ then ~
- improves case
   || a b c equivalest to | (a b c)
   / cases on shelves (insteaf of boxes)
   // || for /
- add assert:null
Check that there is no boxes. Usefull to check if all boxes have been moved
and there is nothing left
- to becomes to> and to^
- Add orientation strategy for different shelves.
  orules take an optional shelf selector argument
  orules+ add rules to the existing ones.
- add shelf splitting and resizing 
   shelf:full  set minDim to maxDim
   shelf:rezise l w h
   split selector l w h statement, split as in :SHELF_SPLIT: then recombine
- add tag:shelves
- add box ranges
   instead of selecting boxes matching a selector
   match boxes from/upto/before and after the given selector.
- add foreach:do execute the given action foreach of the following statements.
   foreach:do action
         st1
         st2
- add foreach:box
  process boxes by group "global" limit
- add bsize resize all selected boxes to
    bsize:max use for each dimension the max across boxes
    bsize:min use for each dimension the min across boxes
    bsize:first use the size of the first boxes
- add swap  boxes (current order to given order)
- tag:for tag boxes temporarily
##  Brick
- add reloading indicator
- Brick: add reloading indicator
   
   



# 2.0.0
- Rewrite WPL parser to allow case blocks
to start a block without extra indentation.
  
-  Misc WPL commands
  foreach:shelf execute given action for each shelf
  in:shelves with:boxes, select shelves/boxes in "context"
  ex /A in:shelves is equivalent to /A in A
  trace:count/boxes
  & can start a then blck
     & A
     & B
     & C
    is equivalent to A B C
    
- Add WPL guards
   empty-(boxes/shelves):(yes/no) activate/deactiavte checking for empty boxes/shelves.
   When active narrowing resulting in an empty set raises an error.
- Brick
  Add keybinding helps
  yank report to clipboard or set to vim/vd
  add bestFitReport
  add overlap checking :CHECKING:

- Base
  add ?? expand operator. value??default

- Rearrange
  add debug tag. If present (@debug=prefix) add debug information (starting with prefix)
  add option to change "dead" tag boxes (@dead=tag)
   
# 1.4.1
- change how limit works
  ^= in limit don't use the default sort order but only the given one
  ^@ forces the use of default order
  WPL first selection uses default order and then not
    so that in effect subsequent limit reuse the previous limit order
- misc in Brick
- add ${id} props
- split by style instead of shelfves in limits

# 1.3.4
Add collapse depth option
Find next and previous box
Add run argument
Misc bux fixes including
- boxes duplicated after diff
- empty start index not working
- save previous tags when replacing box
# 1.3.1
Add fuzzy Stocktake
Refactoring split Base.hs in Move.hs and Tiling.hs
Brick: 
  Add --property
  Display overlapping boxes
# 1.3
Add WPL 
Misc Brick improvement 
- add highlight status
- improve history and diff
Use Megaparser (instead of Parsec).
Seems much faster
# 1.2.6
Add box and shelf selection
Add custom box title using #ctitle
Replace style in stylebar with custom property
Add moves command '%'
# 1.2.5.1
Add reload and cacheRef
# 1.2.5.0
Add history.
Can be navigated through using brick
# 1.2.4.2
Fix shelf order in diagrams.
# 1.2.4.1
Fix diagram bug.
# 1.2.4
Highlight current box
b/B navigate to next/previous box
# 1.2.3
Learn Export option (-x).
Expand moved to -X.
# 1.2.2
Export defaultMainWith
# 1.2.1
Read from stdin
Add direct section to command line (import, delet and tam)
# 1.2.0
Add brick visualisation tool
Add standalone executable

# 1.1.1
Performance optimisation.
HowManyWithDiagonal tries less.

# 1.1.0

Remove auto save point (based on heading levels).
Heading endind wih '!' instead are saving points.

Should remove unnecessay saving points resulting
in less memory usage and better performance
(saving point are time costly).

# 1.0.2

More performance optimisation.
Use boxMap (by style) instead of sequence.


# 1.0.1

Miscellaneous performance optimisation

