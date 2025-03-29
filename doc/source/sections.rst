Section Types
~~~~~~~~~~~~~

.. todo:: sort section alphab

.. todo:: consistent casing in title

.. _tag or pattern:

.. note:: Tag or Patterns

   Some sections accepts a list of tags and patterns as parameters. Those
   tags are usually applied on "success" and applied as well as the tags of
   a specific line. Tag and patterns allows to filter the final tags using
   thes special tags ``@exclude`` and ``@include``.

   ``@exclude`` will exclude all the tags matching the (glob) patterns at
   is right

   ``@include`` will only include all tags matching the (glob) patterns at
   is right.

   This is particularly usefull when import an existing file when some
   selected needs to be imported. Example, given the stock.org file

   ::

      :STOCKTAKE:
      Bay No,Quantity,Style,Length,Width,Height,Orientations
      shelf,1,A#status=active#bg=black,10,20,20,
      :END:

   This will create a box A with the two properties ``status`` and ``bg``.
   Replace ``:STOCKTAKE:``, with ``:STOCKTAKE_@include#stat*``, will only
   the ``status`` property (it is the only one matching the pattern
   ``stat*``. This can be achieved without modifying the file but importing
   it with extra tag

   ::

      :IMPORT:
      file/stock.org#@include#stat*
      :END:

.. _shelves:

Shelves (Mandatory)
-------------------

Describes set of shelves. Should be a CSV with the following header :

::

   name,comment,length,width,height,type[,bottom]

For ``lenght``, ``width`` and ``height`` a mix and a max can be
specified as follow *min*\ ``;``\ *max*. If ``;`` is not present the
same formula will be used for both dimensions. When update both
\_min\_ and \_max\_ or optional. The max of a dimension represents
the physical maximum that can be use and the min the maximum that a
box can be used after. For example when stacking boxes on the floor,
the maximum height will be the ceiling (all box stacks have to fit
under the ceiling) whereas the minimum height will the be maximum
height reachable to handle the box. In other word the last box can
stick out over min height, but no box will be stacked above the
minimum height even though it will fit under the maximum height.
``bottom`` is optional. It represents the height of where the usable
part of shelfstarts (taking the thickness of the shelf itself into
accoun). This is used to determine if a shelf sticks out of the
ceiling (when the ``ceiling`` tag is set). When using formula, the
value taken from the refered shelf will be the top of the shelf (not
the bottom). This is to allow the calculation of stacked shelves
using the

::

   {%-}+10

formula (10 there is the thickness of the shelf)

Special shelves
'''''''''''''''

The two first declared shelves have a special meaning. The first one
is the ``error`` shelf and the second one is the ``pending`` shelf.
The error shelf is used when ever a move try to fit more boxses than
possible in the given shelf. All the selected boxes which doesn't fit
the given location are moved to the error shelf. The ``pending``
shelf is used when creating new boxes. Boxes created using the Boxes
command are created within the ``pending`` shelf. It is best practice
to name those shelves ``error`` and ``pending`` (or equivalent).

Name expansion
''''''''''''''

Groups of similar shelves can be created in one line using name
expansion. Before creating a shelf, the name is expanded into one or
many shelf names. One shelf will be created of each of the resulting
name. Shelf name are split with the ``|`` separator, then when
encountering ``[..]``, name will be expanded by generating a name for
each character (or word) within the bracket. Example

::

   A|B => A B
   A[01] => A0 A1
   A[ABx]X => AAX ABX AxX
   A[A Bx]X => AAX ABxX

More than one ranges can be provided, in this case all combinations
will be generated. Example

::

   [ABC][123] => A1 A2 A3 B1 B2 B3 C1 C2 C3

dimension formula
'''''''''''''''''

Shelf dimension depending on the dimension on another shelf can be
expressed using shelf dimension. This can be useful when shelf are
back to back or one shelf is not physically a shelf but the space
leftover between a shelf and wall. Basic arithmetic can be performed
and the the same dimension of another shelf can be referenced using
``{...}``. Wildcard (``_``,\ ``+`` and ``-``) can be used to modify
the corresponding character of the current shelf itsef. The minimum
and maximum of two values can be computed using the ``&`` and ``|``
operators. ``%`` can be used to match all characters (minus the
number of character after it)\ ``[``..\ ``]`` mirror the correspoding
character within the range.\ ``*`` matches all character up to the
nextExample

::

   name,comment,length,width,height,type
   A1,,150,40,200,
   A2,,100,{_-},20, -- same width as A1 
   B1,,200-{A_},-- length 200 - length of A1
   B2,,200-{A_},-- length 200 - length of A2
   B20,,200-{A%},-- length 200 - length of A20
   C1,,200-{[AC]1},-- length 200 - length of A1
   C2,,{%1}, -- length of C1
   C3,,{%-}, -- length of C2
   B21,,{*2-}, -- lengh of B20 (*2 matches B2)

For A2, ``_`` refers to the first character of the current shelf,
i.e, ``A`` and ``-`` refers to the second character (``2``) minus 1
'> ``1`` For B1, the ``_`` is in the second position and therefore
correspond to the second character of ``B1`` : ``1``. For C1, the
``[AC]`` is transform into ``A`` (C->A, B->B, A->C) An accessor can
be added to a reference to select a particular dimension of the
refered object. This is done with ``{``\ ref\ ``:``\ accessor\ ``}``,
where accessor can be

'  ``length`` min length
'  ``width`` min width
'  ``height`` min height
'  ``Length`` max length
'  ``Width`` max width
'  ``Height`` max height
'  ``bottom`` bottom (height from the ground)
'  ``top`` top (height from the ground)
'  ``usedLength`` used length (depends on boxes within)
'  ``usedWidth`` used width
'  ``usedHeight`` used heigth
'  ``availableLength`` (min length ' used available)
'  ``availableWidth`` (min width ' used available)
'  ``availableHeight`` (min height ' used available)
'  ``AvailableLength`` (max length ' used available)
'  ``AvailableWidth`` (max width ' used available)
'  ``AvailableHeight`` (max height ' used available)

Please note, that ``l`` can be used for ``length``, ``al`` for
``availableLength``, ``AL`` for ``AvailableLength`` etc ...

shelf types
'''''''''''

The shelf type determines the default boxes orientation and filling
strategy. The current shelf style are

-  ``Shelf`` (normal first) : tilted row first
-  ``deadzone`` : allow up column first
-  ``Update`` allows to update an existing shelf (tags will be ignored)
-  ``other``, column first

Update
''''''

Shelves can be updated by redefining it and setting the type to
``Update``. Formulas can use the value of the shelf itself. This
feature allow to tweak a shelf previously defined within a group.
Note that for update, the shelf name is not expanded but filtered
using the normal selector syntax (*box*\ ``/``\ *shelf*).

Tag
'''

Tags can be used to select shelf when doing box moves, but is also
used to group shelves when displaying the summary. Shelves are
grouped using the ``summary`` property. Note that shelves with a
summary value starting with a ``_`` are considered as virtual shelves
and are not taken in to account when calculated used spaces and floor
space. Also, shelves with the ``sep`` tag are seen as separator :
shelves present for layout purpose only and are excluded from the
summary as well as being displayeddifferently.

Special Tag: Ceiling
''''''''''''''''''''

The special attribute ``ceiling=`` set the height of the ceiling. If
the total height + bottom offset is greater than the ceiling value,
the shelf is considered to high. It will be tagged with ``'tooHigh``,
and the height of the shelf will be truncated accordingly to fit the
given ceiling.

.. _layout:

Layout (Mandatory)
------------------

Describes how shelves should be displayed. Shelves are displayed as a
matrix (row and column) of bays. A bay being a set of shelves stacked
together from bottom to top. Each line of the layout section describe
a row. Columns are separated by one or more space and each element of
a bay by a pipe ``|`` Example:

::

   A1|A2|A3 B1|B2
   C D E

``A1|A2|A3`` form a bay of 3 shelves, A1 at the bottom, and A3 at the
top next to a bay of 2 shelves B1 with B2 on top.

.. _stocktake:

Stocktake[\_Tag]
----------------

Describes a set of boxes with their location and eventually
orientation. It is a CSV with the following header

::

   Bay No,Style,QTY,Length,Width,Height,Orientations

Tags provided in the section name, will be applied to the created
boxes. For example, all boxes created in the section
``:Stocktake-tag1-tag2`` will be tagged with ``tag1`` and ``tag2`` If
the ``@throwError`` is given and box doesn't fit in the given shelf.
Instead of moving the box to the error shelf, the planner will stop
and generate an error message.

.. _boxes:

Boxes[\_TagOrPatterns]
----------------------

A set of boxes without initial location. They will be put in the
``pending`` shelf. It is a CSV with the following header :

::

   style,quantity,l,w,h

Tags provided in the section name, will be applied to the created
boxes.

.. _moves:

Moves[\_TagOrPatterns]
----------------------

Describes a set of moves boxes to shelves. The first column describe
a set of boxes to moves to a set of shelves. If multiple shelves are
given, the Planner will fill the shelf in the given order and use the
optimal orientation. If all boxes can't fit the given shelves, the
excedendatary boxes will be moved to the **error** shelf. It is CSV
with the following header:

::

   stock_id,location[,orientations]

Please not the stock_id and location are in fact boxes and shelves
selectors (see selectors sections). An orientation can be given
optionnaly.

Filling order, Exit on top
''''''''''''''''''''''''''

When moves boxes to a new set of shelves, shelves are filled by
alphabetical order. For example the command

::

   :Moves:
   stock_id,location
   ,A|B|C

Will move all boxes to the shelves A, B and C starting by filling A,
the filling B and so on. Boxes are stacked in column form left to
right. It is however sometimes desirable to carry on filling the same
column on the next shelf rather than creating a new column on the
current shelf. This can be achieved by specifying the "exit on top"
option by starting the location with ``^``

::

   :Moves:
   stock_id,location
   ,^A|B|C

The code above, will fill the first colum into shelf A, then a column
in B and then C. When the first column in C is full, it will start a
2nd column in A, then B etc ... Separating shelves with `` `` will
indicate them as separate bay.

::

   :Move:
   :Moves:
   stock_id,location
   ,^A|B C

The code above, will fill the first columen in shelf A, then a column
in B and then restart in A and so on until there is not column left
in A and B. It will then start filling up C. (This syntax is similar
to the syntax of the **Layout** section).

Partition Mode
''''''''''''''

When filling a shelf with boxes, the default strategy is to use to
either fill the shelf on the right of the existing boxes or the top
(which ever gives the best result). This works fine most of the time
but might result in available spaces beeing "shadowed" by existing
corner. In the following configuration, ``#`` represents existing
boxes.

::

   |     .
   |a A  . B
   |##.......
   |##   . 
   |## C . D
   |######_d___

The default strategy will fill either d,D and B (filling at the
right) or a,A and B (filling on top). The C zone is shadowed. To put
a box in C, will requires to try every available rectangles which
will makes the planner very slow. However, if needed, the partition
mode (which parts of the shelf needs to be filled) can be specified
before the shelf name (as with "exit on top"). One or more partition
mode can be specified as follow: - ``~`` Above only (in the example
above: a A B) - ``:`` Right only (in the example above: d D B) -
``%`` Best effort (excluding above corners a and d) C A D B Not
specifying anything is equivalent to ``~:`` Another possibilty is to
empty the shelf(ves) and fill the shelves with the existing boxes and
the new ones. In that case, we might want to resort all boxes (old
and new) or keep them in the orignial order (old then new). - ``@``
Sort old and new boxes - ``+`` old then new boxes in original order
Example

::

   :Moves:
   stock_id,location
   ,%:~A -- tries a A B, A C B D and d D B
   ,A -- equivalent to ,~:A. Tries  a A B and d D B
   ,%:A -- tries A B C D and d D B
   #!,@A -- resort content of shelf A

Tagging
'''''''

Tags provided as section parameter will be applied to the boxes
**successfully** moved whereas boxes which couldn't be moved (not
enough space in the destination) will see those tags negated. For
example, let's say that we are trying to move 3 boxes in a shelf with
``:Moves_moved_-error`` but only the first 2 are moved successfully,
the two first boxes with see ``moved`` and ``-error`` applied (which
result in adding the tag moved but remove the tag error, whilst the
last box will see the ``-moved`` and ``error`` apply. As a result the
two first boxes will have the tag ``moved`` and the last one the tag
``error``.

Empty selection
'''''''''''''''

Sometimes, a selector doesn't select anything. This can be because of
a typo or because a box is not present anymore in the warehouse. To
detect such cases setting the tag ``@noEmpty`` will raise an error
(and stop) if there is nothing to moves.

.. _tags:

Tags[\_TagOrPatterns]
---------------------

Tags allow boxes to be selected (via selector) to be either moved or
tagged but also change their behavior (colour, priority, etc ...) via
properties. A Tag can be removed by setting with ``-`` The body is a
CSV with the following header

::

    stock_id,tag

Example

::

   :TAGS:
   stock_id,tag
   ,#tag1
   A,#-tag1
   #tag1,#bg=red

The first line, tag all boxes with ``tag1``. The second line remove
``tag1`` from the A boxes. The last line set the background property
of the box tagged with ``tag1`` to red.

.. _moves-and-tags:

MAT[\_TagOrPatterns] (moves and tags)
-------------------------------------

Allows to move and tag at the same time a set of boxes. This can be
faster and less verbose than creating a move and a tag section. Tags
needs to start with a ``#`` and location CAN start with ``/``

::

   stock_id,location#tag[,orientations]

Example

::

   :MAT:
   stock_id,location#tag
   #new,A#-new

Moves all new boxes (with the new tag) to A and unset the new tag.
Note that tag parameters will also be added to the "per-line" tag. As
in ``:Moves`` tags are applied positively to boxes successfully moved
and negatively to leftover.

.. _shelf-tags:

SHELF_TAGS (shelf tags)
-----------------------

Tag the shelves containing selected boxes. Tags can be used to
specify the styling of a shelves. Example

::

   :SHELF_TAGS:
   stock_id,tag
   A,tag -- tag all shelves containing A
   /S,tag -- tag shelves with name S
   /#sep,fg=blue -- set the foreground of all shelves having the `sep` tag
   #new/#top,tag -- tag all 'top' shelves containing a items with the new tag

Update shelves
--------------

Updates the dimensions of the shelves containing selected boxes. Can
be used to readjust shelves and their neighbour according to the
space use by its content. Example

::

   :UPDATE_SHELVES:
   stock_id,l,w,h,bottom,tag
   /A,{A}+{B:availableLength},, -- expands A with B free space
   /B,{B:usedLength},, -- shrink B to its content

Expand A and shrink B by the same amount (so that A+B stays the
same),

.. _shelf-split:

SHELF_SPLIT (shelf split)
-------------------------

Split a shelf performing guillotine cut. The dimension columns
specify the dimension to cut. it can be any formula with reference
another shelf or objet. For each object the dimension corresponding
to the column will be used, unless accessor is specified (see
`dimension formula <#dimension-formula>`__) If a box selector is
specified, the dimension of the first box found can be used. Extra
object

-  ``{}`` or ``{%}`` or ``{shelf}`` the shelf itself
-  ``{content}`` dimension of bounding box of box inside the shelf
-  ``{=}`` or ``{|}`` etc box with the given orientation
-  ``{*}`` box with first possible orientation

The split shelf is resized and the created ones have the same name
with a 3 letter suffix separated with ``/`` index added. Example

::

   :SHELF_SPLIT:
   stock_id,location,length,width,height
   ,A, {%}/2, , -- cut A in 2 of half the length : A A/baa
   ,A, {%}/4 {%}/2, 50, 10 -- cut A in 12 3x2x2 A A/baa A/caa A/bba A/cba ...
   ,A, {B:height},, -- cut length using shelf B height
   box,A,{|}*2,, cut at two time the lenght of box with | orientation

.. _shelf-join:

SHELF_JOIN (shelf join)
------------------------

Shelves which have been split can be join back together. The selector
must refer to the base shelf (not the split ones)

::

   :SHELF_SPLIT:
   stock_id,location,length,width,height
   ,A, {%}/2,{%}/2, -- create A/ba A/bb A/ab
   :END:
   :SHELF JOIN:
   location
   A -- join A/ba A/bb and A/ab to A
   :END:

.. _clones:

CLONES
------

Allows to duplicate the given boxes. Used in conjunction with
`Deletes` it can be used to do slotting by creating fake boxes
(ghosts) which will make sure a slot is full and the remove later.
For example

::

   :Clones:
   stock_id,quantity,content'tag
   A^1,4,#ghost

or (note the position of the tag ``ghost``

::

   :Clones-ghost:
   stock_id,quantity,tag
   A^1,4,

will create 4 boxes with the tag ``ghost`` for each colour of A.
``^1`` makes sure we are doing the cloning operation once per colour.
Without it, we will have 4 clones for every box.To create slots of
for, we could move all As by 4 with

::

   :Moves:
   stock_id,location
   A^4,destination

No more that 4 of each colour will be moved using the ghosts if
necessary. We can then delete the ghost using ``:Delete:``

::

   :Delete:
   A#ghost

The content of a new box can specified before the tag. For example

::

   :Clones:
   stock_id,quantity,content'tag
   A#'BLK^1,4,RED#ghost

Will create 4 red boxes for each BLK. By default only tags that are
specified either as default tag or for each line will be applied to
the box. To copy a box and ALL its tags, start the content/tag
specification with a ``!``.

::

   :Clones:
   stock_id,quantity,content'tag
   A^1,4,!#ghost
   A^1,4,!new-content
   A^1,4,!new-content#ghost

.. _delete:

DELETE
--------

Delete the selected boxes.

::

   :Delete:
   A#ghost

Delete all ``A`` boxes with the ``ghost`` tag.

.. _transform-tags:

Transform[=properties] (transform tags)
---------------------------------------

| Allow to use POSIX regular expression to subsitute existing tags
  into new ones. Depending on if properties are given or not, the
  behavior will be slightly different. Without properties, each tag
  of the selecting boxes are matched against the pattern. A set of
  new tags is generated by substituing the pattern with the
  substitution string which is then splitted using ``#``. Other tags
  can be removed by generating a *negative* tag (using ``-``). The
  original tag is not deleted but can be done using ``-\0``.
| If properties are given, the transformations will only apply to the
  values of those properties. This should be faster but doesn't allow
  renaming or deleting a tag/property. It is a CSV with the following
  header

::

   stock_id,pat(tern),sub(stitue)

Examples

::

   A,black,blue --> add the blue tag to each box of type A
   ,black,blue#-black --> replace black by blue
   ,black,blue#-\0 --> replace black by blue. (remove black)
   ,^[[:upper]],-\0 --> remove all tags starting with an uppercase

Group (using \`(..)\`) can be use to extract substring

::

   ,(..)-(..),\2:\1 --> add BB:AA from the tag AA-BB

Properties and virtual tags are expanded in the regexp itself.
Example

::

   :TAGS:
   stock_id,tag
   ,shelfname=$shelfname -- set shelfname property using shelfname attribute
   :END:
   :TRANSFORM:
   stock_id,pat,sub
   ,location=.*$[shelfname],unmoved -- detect boxes which haven't changed

In this example, we need to use an intermediate property
``shelfname`` because the name of the shelf can contains ``/`` which
are replaced by ``'`` when the tag is set. For example if object A is
in location ``W/2``, it will have a tag ``location=W'2`` (instead of
``location=W/2``). ``$shelfname`` expands to ``W/2`` whereas the
value of the shelfname propery will be W'2 (``shelfname=W'2``). This
behavior might be fixed and therefore this workaround not necessary
in a future versioin. To detect moves only if the the 3 first letter
of the shelf name have changed :

::

   :TAGS:
   stock_id,tag
   ,shelfname=$shelfname -- set shelfname property using shelfname attribute
   :END:
   :TRANSFORM:
   stock_id,pat,sub
   ,shelfname=(...).*,shortshelf=\1
   ,location=(...).*,shortloc=\1
   ,shortshelf=$[shortloc],unmoved -- uses the value of shortloc property
   :END:
   :TRANSFORM_shortshelf:
   stock_id,pat,sub
   ,A,B -- rename the value of short shelf from A to B
   :END:

.. _orientations:

Orientations
------------

Specifies the boxes configuration within a shelves (if they are
stacked up, on the side, how many etc). Boxes of a given style can be
given different configuration for different shelves by specifing the
shelf in the box selector. This is a CSV with the following header:
``stock_id,orientation``\ Orientation must have the following format
``no-diagonal stackin-limitg orientations`` Example:

::

   TSHIRT/#top,^
   TSHIRT,!|=

All T-shirt on top shelves (with the tag ``top``) are up, whereas
T-shirt in other shelves are being laid on the side or the other with
no diagonal allowed.

Orientations
''''''''''''

::

   * -- all 
   % -- default orientations
   ^ -- up
   = -- tilted forward
   > -- tilted right
   | -- tilted forward & right
   ' -- rotated up
   @ -- rotated side

max stacking specification
''''''''''''''''''''''''''

By default, boxes are stacked using only one level of depth. This
way, no boxes hide behind others and so all boxes are visible. To
enable the use of multiple depth and allow boxes to hide each other,
a minimum and max depth can set (before) A maximumn limit for height
and width (actual bay length) can be specified (but no minum). Some
or all of the limit can be specified as follow
``depth | depth x height | lenght x depth x height`` Example

::

   ,1:4 -- allow up to 4 depth level
   ,1: -- use a mininum of 2
   ,4 -- similar to 1:4
   ,4^ -- up to 4 levels, stacking boxes up
   ,1x2 -- max depth 1, max height 2
   ,1x2x3 -- max width 1, max depth 2, max height 3
   ,xx3 -- max height 3

.. _colour-map:

Colours
-------

Defines a map colour name to colour value. The value can be either a
existing colour name or a RGB value (without the ``#``). It is a csv
with the following header :

::

   name,value

Example

::

   :COLOURS:
   name,value
   red,ff0000
   good,green
   :END:

.. _import:

Import
------

Allows to import whole planner files either from existing files or
generated on the fly from an import dispatch provider (such as ``Fames``). Each line correspond to an
import and will be replaced with the result of the import. Some
imports accepts tags. Tags are given by "tagging" the import line
using ``thing_to_import#tag1#tag#...``


-  ``files/``\ pattern[``#``\ exclusive-pattern] Import all files
   matching the glob pattern. Files are local to the planner template
   directory.Tags can be used to filter out some file matched by the
   original pattern. Example

   ::

      files/Base.org --  Base.org file
      files/Base/* -- all file present in the Base folder
      files/Base/*#moves.org -- all file present in the Base folder except Base/moves.org
      files/Base/moves.org -- Base/moves.org only

-  ``file/``\ pattern[``#``\ tags] Import one file matching the glob
   pattern. Files are local to the planner template directory.Tags
   are added to each sections of the corresponding file Example

   ::

      file/Base/container.org#C1 -- Call container and #C1 to all sections within it.

.. _rearrange:

Rearrange[\_TagOrPatterns]
---------------------------

Reposition the boxes in a cyclic manner to rearrange them, ensuring that
boxes marked with the tag ``#dead`` are either eliminated or relocated
to the end. This process involves filling the resultant gap by shifting
the necessary boxes.

By default, the reorganization occurs exclusively within groups of
identical content. This emulates the refilling of vacant slots with
boxes of the same content, typically sourced from an alternate shelf,
often situated at the top. The retention of dead boxes facilitates the
identification of the intended purpose of the empty slot.

Moreover, instead of relocating all the boxes, the realignment can be
executed in such a way that only a minimal number of boxes need to be
shifted, allowing some boxes to remain in their original positions.

::

   boxes,actions

``actions`` is a list of box selectors indicating where to shift the box
with.The syntax is as follow

::

    [-%/] [!]action1 '>' [!]action2 '>' ...

Within the actions, box selectors exclusively choose a subset of the
main box selector. They are complete box selectors
(``box-pattern[/shelf-pattern]``), unless the entire action begins with
``/``, in which case, all selectors will be shelf selectors (i.e., boxes
from the full box selector in the selected shelf).

If an action begins with ``!``, boxes that remain in the \*selection\*
will stay in their current positions.

If an action begins with ``%``, boxes will be treated as a whole rather
than by content.

If an action begins with ``-``, ``#dead`` boxes will be deleted.

Example (letters indicate content, lowercase denotes dead boxes):

::

   :RAR:
   selector,actions
   T-Shirt,/#top > #bottom -- Shift each box from the top shelf to the bottom shelf based on color
   --
   --        A2 B3 C2 C3| #top       a1 b1 C3 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B2 B3 C2
   --
   T-Shirt,/#top > !#bottom -- Same, but the box on the bottom shelf (B2) remains in place
   --
   --        A2 B3 C2 C3| #top       a1 b1 C3 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B2 B3 C2
   --
   T-Shirt,/#top > !#bottom -- B2 and C3 remain in place
   --
   --        A2 B3 C2 C3| #top       a1 b1 c1 C3
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B3 B2 C2
   --
   T-Shirt,%/#top > #bottom -- All boxes shift
   --
   --        A2 B3 C2 C3| #top       C3 a1 b1 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    B2 A2 B3 C2
   --
   T-Shirt,%/#top > !#bottom -- B2 remains in place
   --
   --        A2 B3 C2 C3| #top       C3 a1 b1 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B2 B3 C2
   --ND:

.. _freeze-order:

FreezeOrder[\_TagOrPatterns]
-----------------------------

Freeze the order boxes are stored internally accordingly to the order
boxes are selected. Shouldn't change much but might improve performance
or provide with a stable order.

::

   :FreezeOrder:
   selector
   * -- resort everything according to default priority
   :END:

.. _wpl:

.. todo:: WPL
   
.. _check-shelves:
   
.. todo:: check shelves
   
.. _update-shelf:
   
.. todo:: update shelf
