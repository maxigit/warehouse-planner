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

