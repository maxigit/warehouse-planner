Displays a Planner (a visual view of the warehouse with its content).
The planner is a set of files using a subset of Emacs org-mode syntax
(mainly headings and drawer)s. The use of of org-mode is for convenience
only and provide folding of syntax highlighting for free. Files are read
from the given subdirectory (if any) and/or read from the text area(s).
A planner file is made of different sections. Each section corresponds
to an org-mode drawer (starting and finishing with ``:``). A section can
be used to described shelfves, boxes but also move or tag shelves.

Boxes and shelves selector
~~~~~~~~~~~~~~~~~~~~~~~~~~

Most operations, (moving and tagging) operates on boxes. To do so, a set
of boxes is selected and then the operation is applied. The main way of
selecting a box is by specifying a style, which will select all boxes of
the given style. However, more complex selection can be selecting boxes
by tags and locations (as well as location tag). The format of a box
selector is the following

::

   style pattern # tag(s) pattern // location pattern # location tag

Example

::

   TShirt => all boxes with the style 'TShirt'
   #white  =>  all boxes with the 'white' tag
   TShirt#white => all boxes with the style 'TShirt' AND the white tag
   /A => All boxes in the A Shelves
   #white/A* => All boxes in a shelves with the name starting with 'A' shelves AND The white tag
   TShirt/#top => ALL Tshirt on the shelf with the 'top' tag.

Glob pattern
------------

When selecting boxes or shelves, wildcard can be used to match the
name and the tags of the box or the shelf using `UNIX glob
pattern <https://en.wikipedia.org/wiki/Glob_(programming)>`__.The
``|`` operator can also be used to give a list of alternatives.
Example, given the location

-  A01
-  A02
-  A11
-  A12
-  B1
-  B2
-  B3

::

   A* => A01 A02 A03 A11 A12 A3
   ?? => B1 B2 B3 -- two characters only
   B[12] => B1 B2 -- 3 is not in the range 1-2
   A?1|B2 => A01, A11 , B2

Please note that the syntax used to create group of shelves is only a
subset of the glob pattern syntax. Even though pattern globing can
only create disjunction (OR), conjunction (AND) can be achieve using
tags. Example

::

   *ED|*XL => select things which are red OR extra large..
   *RED,red => tag all RED thing
   *XL,xl => tag all XL things
   #red#xl => select thing which are red AND extra large.

style pattern
-------------

A glob pattern on the name of the box.

tags
----

Starts should start and be separated with ``#``. Only boxes with ALL
tags will be selected. Preceding a tag with ``-`` means boxes wich
don't have this tag. Example

::

   TShirt#red#-XL => all red tshirt , except the XL Ones.

Location pattern
----------------

A glob pattern on the name of the shelf.

Location tag
------------

A glob pattern on the tag to select a shelf.

number restriction
------------------

The ``^`` symbol is used to select only a certain number of boxes per
variant/content, per shelf, and in total

::

   ^ content ^ shelf ^ total

Example

::

   TShirt^^^1 => the first box containing a t-shirt.
   ^1 => The first box of each variations (colour)
   ^2^3 =>  The two first boxes of each variations, with a maximum of 3 per shelves.

Each number specification can also specify a starting number and some
tags and attribute. If tags/attributes are specified, they will be
used to sort the boxes before decidind which are the n first. If a
"shelf" tag is specified the value of the tag will be used to group
boxes instead of using the shelfname. Example

::

   TShirt^^^2:2 => The second box containg a t-shirt
   TShirt^^^1:2 => The first, second box containg a t-shirt
   TShirt^^^[price]1 => The box with the lowest price
   TShirt^^^-[price]5 => the five box with hight price
   TShirt^^^[price]{coordinate} => sort boxes by price tag and coordinate attributes

Priority and selection order
----------------------------

Boxes are selected in semi-arbitrary order which can be modified
setting up priority. The order in which box are selected affect the
way boxes are actually stacked on shelves but also which boxese are
selected when using number restriction (see above). By default boxes
are selected in order by

-  global priority
-  style name (ascending)
-  style priority (priority within style)
-  content name
-  content priority (priority within content)

By default, all priorities are set 100. Priorities can be modified by
assigning the special tags, ``@global``, ``@style``, ``@content`` but
also any other tag using the ``^[tag]`` notation (see number
restriction).

-  ``@global`` global priority. Can be used to move a box first.
-  ``@style`` priority within the same style. Can be use to move a
   box at the beginning of a style.
-  ``@content`` priority within box of the same content (styles and
   variations). Can be

For example, given fox boxes, A-Black, A-Red, B-Black, B-Red. Boxes
will be stacked in the following fs order

-  B-Black
-  B-Red
-  A-Black
-  A-Red

or

-  A-Black
-  A-Red
-  B-Black
-  B-Red

A and B having the same global priority, the system is free to start
with A or B. However, content (Black and Red) are sorted
alphabetically. To ensure that, A is processed before B. We need to
assign it a priority < 100 to A (global priority) with

::

   A,@content=1

To get B-Red boxes before B-Black boxes we can assign it a priority
(style priority)

::

   B-Red,@style=1

Settings those two priorities will result in the following order :

-  A-Black # @style=100 @content=1
-  A-Red # @style=100 @content=1
-  B-Red # @style=1 @content=100
-  B-Black # @style=100 @content=100

The content priority could be used for example, to select which one
of the B-Black boxes to get first.

Set and use reference
---------------------

Part (or all) of a selector can be used as reference to be reused in
the following lines. A reference can be set by enclosing it between
``&[`` and ``]&``. It will be used as a base selector for all the
subsequent lines of the same sections. This is somehow equivalent to
select thoses boxes and then apply subsequent filter to the
"selection".

::

   &[A]&,action   -- set A as reference and apply action to all As.
   #tag,action2 -- equivalent to A#tag,action2
   ]&,B,action3 -- reset the reference and apply action3 to all Bs

The reference can only capture a part of the initial selector

::

   &[A]tag1,action   -- set A as reference and apply action to all As with tag1.
   #tag2,action2 -- equivalent to A#tag2,action2
   &[A#tag1]&,action   -- set A#tag1 as reference and apply action to all As with tag1.
   #tag2,action2 -- equivalent to A#tag1#tag2,action2

If only one delimiter is present, reference will set as follow

::

   &[A <=> &[A]&
   A]&  <=> &[]A&
   A&[extra  <=> A&[extra]&
   A]&extra  <=> &[A]&extra

This at the moment only works for the box selectors of the section
related to moving or tagging boxes.

Virtual tags
~~~~~~~~~~~~

Virtual tags are tags which are not set by the user, but automatically
set depending the box content and/or the box itself. It allows to select
style by variations (``'``\ content) and dimensions (in 1/10th of mm)
(``'l``\ length, ``'w``\ width,\ ``'h``\ height ). Example

::

   TShirt#'l3500 => all T-shirt with boxes with a length of 35cm.
   TShirt#'RED  => all red T-shirt (with a content of Red)

Virtual tags can also be used to change the dimension of a box. Setting
for example the tag ``'h3400`` will change the box size to 34 cm. If
more than one values is assign to a dimension, the sum of all is used.
This can be used to add a subtract a value to dimension. Example

::

   #'l3500,#'l=+50 => add 50 to the length
   #'l3500,#'l=+-50 => subtract 50 to the length

Note that ``=+-50`` adds a value of -50 (see
`Properties <#info-section-property>`__) to the dimension.

Properties
~~~~~~~~~~

Tags come in two flavours, tag (a simple word) or property (name
``=``\ value). The main difference between both is assign a property
value erase the previous one. For example, adding the tag ``color-red``
then ``color-green`` result in the box being tagged with two tags
``color-red`` and ``color-green``. In the contrary, adding the tag
``color=red`` then ``color=green`` result in the box being tagged with
one tag ``color=green``. In this case, ``color`` is a property therefore
tagging with ``color=green`` change the value of color (previously red)
to green.Example

::

   A,prop=first --> A boxes have the tag 'prop=first'
   B,prop=second
   ,XXX$[prop] -- create XXXfirst tag for A and XXXsecond for B

Tags from the containing shelf can be accessed via ``$/``\ tag\ ``/``
Properties can have more than one value, which can be used for example
to mix colours. Values are separated by ``;`` and can be added or
removed using ``=+`` or ``=-`` For example

::

   color=red#color=green => color=green (green override red)
   color=red#color=+green => color=green;red (add green red)
   color=red;green => color=green;red (set two values : red and green)
   color=red;green,#color=-red => color=green (remove green for red and green

Rendering properties
--------------------

Some special property are used to control the rendering of of boxes and
shelfves. For example assigning ``bg=red`` will set the background of
the box to red. Those properties are

-  ``bg`` background colour (shelves and boxes)
-  ``fg`` foreground (text) colour (shelves and boxes)
-  ``BG`` background colour of the maximum dimension (shelves)
-  ``border`` border colour (shelves and boxes)
-  ``circle``..\ ``circle4`` display a big circle of the given colour(s)
   on the box. Ideal to mark boxes. (boxes only). Displays pies if more
   than one colour is given.
-  ``title`` text to display instead of the box or shelf name (shelves
   and boxes)
-  ``bar-title`` text to display in the box bar (for each depth) or
   shelf bar.
-  ``bar-bg`` colour of the shelf bar (shelf bar).
-  ``bar-fg`` colour of the text of the shelf bar (shelf bar).
-  ``no-bar-gauge`` if present, disable the colour gauge behind the box
   bar or in the shelf bar.
-  ``bar-gauge-x`` if present, offset the bar gauge by x times the box
   index.
-  ``bar-gauge-y`` if present, offset the bar gauge by y times the box
   index.

.. _colours-1:

Colours
--------------------

All property colours accept more than one colours (separated by ``;``)
which are colour names their hexadecimal description (for example
"white" or "ffffff" ) . If multiples colours are given they will be
mixed. However, each colour can only appear once (in the list of
colour). It is however possible to give more weight to a colour by
prefixing its name with a underscore. For example

::

   black#white => mid gray
   black#_black#white => dark gray
   black#_white#white => light gray

Colour palettes defined in haskell `palette
package <http://hackage.haskell.org/package/palette>`__ can be used. A
colour is represented by the palette name, the number of colour it
contains (if the palette of different variant) a dash and the colour
index starting a 1. Example

::

   YlGn4-1 -- first colour of the 4 colours variant of the YlGn palette.
   YlGn9-8 -- 8th colour of the 9 colours variant of the YlGn palette.
   wheel24-2 -- 2nd of a colour wheel

The palettes available are the one from the `Brewer
Set <http://hackage.haskell.org/package/palette-0.3.0.2/docs/Data-Colour-Palette-BrewerSet.html>`__
and the
`ColorSet <http://hackage.haskell.org/package/palette-0.3.0.2/docs/Data-Colour-Palette-ColorSet.html>`__.
Note that ``rybColor`` is name ``wheel24`` and the ``d3Colors`` 2 and
for used double indexing Example

::

   d3Colors44-1  first  colour of the bright version of d3Color4
   --       ^ bright
   d3Colors41-1  first  colour of the dark version of d3Color4
   --       ^ dark

Indexing property values
--------------------^^^^

Tag values can be indexed within the range of all the values taken by
this tag across all the boxes. The index is the index of the value of
the values sorted alphabetically whereas the rank is the index of the
value sorted by number of occurance. For example, if we have 1 boxes
scanned by operator A, 3 boxes by operator B And 2 by operator C

::

   Box1#operator=A
   Box2#operator=B
   Box3#opertor=B
   Box4#opertor=B
   Box5#opertor=C
   Box6#opertor=C

The index of A is 1, B is 2 and C is 3, whereas the rank of A is 3 (only
1 occurrence), B is 1 (3 occurrences) and C is 2. To get the rank or the
index the special property syntax is ``$index``\ op\ ``[``\ tag\ ``]``
and ``$rank``\ op\ ``[``\ tag\ ``]`` where op is an optional operator
``-`` ``%`` or ``^`` followed by an integer.

::

   -n -- limit the index to n 
   %n -- cycle within n value (index modulo n)
   ^n -- map all indexes to the range 1-n

Example, given the previous boxes

::

   $index[operator] -- A -> 1,  B ->  2, C -> 3
   $rank[operator] -- A -> 3,  B ->  1, C -> 2
   $index-2[operator] -- A -> 1,  B ->  2, C -> 2 (limit to 2)
   $index%2[operator] -- A -> 1,  B ->  2, C -> 1 (cycle to 2)
   $index^5[operator] -- A -> 1,  B ->  3, C -> 5 

This can used to given a different colours to each boxes depending on
the operator

::

   bg=Spectral10-$index%10[operator]

Where Spectral10 is the 10 colours variant of the Spectral palette Of
course different colours can be mixed

::

   bg=Spectral10-$index%10[style];Greys4-$index^4[style]

Let's suppose we have 40 different styles. $index%10 will return 1 for
1, 11, 21 etc ... But $index^4 will return 1 for 1-10, 2, for 2-19 etc
... This way each of the 40 styles have a different colours. For dates,
the
``op``\ ``[``\ ``tag``\ ``]``\ ``transform the date not into a index but to an integer but the number of days ago (from today). Operators have a different meaning (subject to change)``

::

    $ago[date] -- number of days ago
   $ago-0[date] -- number of years ago
   $ago-n[date] -- normalize  date range To n
   $ago-n[date] -- normalize  date range To n
   $ago%n[date] -- year moduln n
   $ago^0[date] -- within last week (1) , month (2), 3 moths (3), 6 months (4), year (5) , 3 years (6), more (7)$ago^n[date] -- log so that the current date range go from 1 to n

The index/position o the box in the current selection (taking ordering
into account) can be used with ``$n``\ op\ ``[``\ [format]\ ``]``,
``$select``\ op\ ``[``\ value1\ ``|``\ value2\ ``|``...\ ``]``,
``$cycle``\ op\ ``[``\ value1\ ``|``\ value2\ ``|``...\ ``]``. This can
be used to generate different values for each boxes

::

   --                       Box1    Box 2  Box3   Box4
   $n[]              -- 1       2      3      4
   $n*3[]            -- 3       6      9      12
   $n%3[]            -- 1       2      3      1
   $n^3[]            -- 1       2      3      3
   $n[%02d]          -- 01      02     03     04
   $select[red|BLUE] -- red     BLUE   BLUE   BLUE
   $select[red|BLUE|]-- A       BLUE      
   $cycle[red|BLUE]  -- red     BLUE   red    BLUE

Evaluation
----------

Tags and properties can be evaluated in different way resulting in
different values. The syntax is the same for properties and tags

-  ``?[then][:[:else]]`` test if the value is present.
-  ``:[start][:[end]]`` extract substring. ``start`` and ``end`` can be
   a (negative) number.In that case, it represent how many character to
   drop on each side. If it is a char, strip until (from) the given
   char.
-  ``%format`` integer formatting (as in
   `printf <https://hackage.haskell.org/package/base-4.18.0.0/docs/Text-Printf.html>`__)

For example, given box inh shelf ``E01.03/2``, and ``#top`` is present

::

   $[top?]  => top
   $[bottom?]  => ''
   $[top?on]  => on
   $[bottom?Y:N]  => N
   ${shelfname} => E01.03/2
   ${shelfname:.} => 03/2
   ${shelfname::/} => E01.03
   ${shelfname:.:/} => 2
   ${shelfname:2} E01.03
   ${shelfname:-2} /2
   $[@global%05d] 00100

This can be used in ordering as well.

Breaks And Slots
~~~~~~~~~~~~~~~~

Normally, when moves boxes to shelves, shelves are filled continuously
without any break betweenboxes of the same dimensions. Breaks can be
introduced using special break tags. By tagging a given box with a break
tag, the box is guaranteed to either start a new shelf, a new slice (row
or column depending of the shelf filling strategy) or a new slot (no box
"behind"). The corresponding tags are

-  ``@start=new-shelf`` first box of an entire shelf
-  ``@start=new-slice`` first box of a row/column
-  ``@start=new-slot`` no box behind

Box attributes
~~~~~~~~~~~~~~

Certain attributes like the current location or orientation of a box can
be used to set a new tag with the corresponding value. The following
attributes are available.

-  ``${shelfname}`` # current shelf
-  ``${shelftags}`` # tag of the current shelf
-  ``${fit}`` # How the box fits in the shelf : fit, tight, or out
-  ``${orientation}`` # current orientation
-  ``${style}`` # current box style
-  ``${content}`` # current box content (or colour)
-  ``${boxname}`` # box style + content
-  ``${dimension}`` # box dimension in cm
-  ``${offset}`` # box offset (within the shelf) in cm
-  ``${coordinate}`` # box coordinate (as if row and column of similar
   boxes) (start at 1)
-  ``${ol}`` $ length coordinate
-  ``${ow}`` $ width coordinate
-  ``${oh}`` $ height coordinate
-  ``${@content}`` $ content priority
-  ``${@style}`` $ style priority
-  ``${@global}`` $ global priority

Example

::

   /pending,loc=$shelfname  => All boxes in the pending location will be tagged with "loc=pending".

Mop export
~~~~~~~~~~

The location of all the boxes can be exported to MOP (via the
generateMOPLocation). By default, locations are given for each style
(regardless of the content) in the form of the pattern matching all used
shelves. The location of a particular variant (style + content) can
exported separately using the tag ``mop-exception``. Boxes can also be
excluded using the tag ``mop-exclude``. This is particularey usefull to
exclude shelves which shoudn't be taken into account. Example, to
exclude all shelves starting with ``_`` (shelves filtered from the
summary report)

::

   :TAGS:
   stock_id,tag
   /#_*,mop-exclude
   :END:

Arbitrary comments can also be added to a box using the tag
``mop-comment=``\ *``your comment``*. In that case, variant with comment
will be exported separately (like with ``mop-exception``) except the
location will be the location of all the box of the same style (instead
of the exact location of the box itself). To have a global comment (the
same across all variant of the same style) without generating a line per
variant, the tag ``mop-no-exception`` can be used. In case many boxes of
the same group have different comments, the one of the first box will be
used. To force a particula box to be used force, ``@content`` and
``@style`` can be used. Finaly, shelves with the tag ``mop-priority``
will be used first when deciding which shelf is to display first.
Shelves with the tag ``mop-excluded`` will be excluded.

Generic report
~~~~~~~~~~~~~~

A generic report can generated using special tags. All boxes will be
grouped first using the special tag *``report``*\ ``-group`` and then
*``report``*\ ``-key``. For each group (having the same key), a line
will be displayed with the content of *``report``*\ ``-group`` (for the
first level of grouping) and the *``report``*\ ``-value`` property (for
boxes grouped by *``report``*\ ``-key``). The ``report`` prefix can be
changed by setting an alternative prefix in the parameter field. This
allows multiples report to be defined within the same plan. The
following group attributes will be expanded :

-  ``${count}`` : number of boxes within the group
-  ``${shelf-count}`` : number of different shelves
-  ``${locations}`` list of shelves (compacted)
-  ``${shelves}`` list of shelves
-  ``${total-volume}`` : total volumes in m^3
-  ``${style-count}`` : number of different styles
-  ``${content-count}`` : number of different content
-  ``${dimensions-count}`` : number of different dimensions
-  ``${orientations}`` : different orientations
-  ``${count-orientations}`` : number of different dimensions
-  ``${today}`` : today's date with the following format ``YYYY-MM-DD``

Symbols would can't be used without being interpreted by the parser can
be expanded using

-  ``${hash}`` ``#``
-  ``${comma}`` ``,``
-  ``${divide}`` ``/``
-  ``${dollar}`` ``$``

Example, to generate a valid TAG File tagging each box using its unique
barcode tag with its location

::

   :TAGS:
   stock_id,tag
   ,report-key=$[barcode]
   ,"report-value=$[report-key],$location"

Note the presence of quotes wich allow a comma to be used inside the tag
field and the use of ``$[report-key]`` to display the barcode in the
report

Best reports
~~~~~~~~~~~~

Best boxes, best shelves and best available shelves computes
respectivily the best boxes to go in a given shelf, the best shelves to
hold the given boxes. The boxes/shelves to analyses should be set in the
parameter field. If the field starts with ``!`` then depth of shelf and
boxes is not used in displaying the used ratio. Fol best shelf reports,
a set of boxes and shelves can be given using the
``boxSelector[,shelfSelector[,orientationRules]]`` syntax. In that case,
only the given boxes and shelves will be taken into account for the
report.

Report Selector
~~~~~~~~~~~~~~~

Some reports like ``GenerateMovesWithTags`` accept a box selector as a
parameter. If an order is given, see (selection order), the specified
order of the boxes will be used. Also box tags shown in the report can
be filtered using ``@exclude`` and ``@include``. For example

::

   T-Shirt^^^[barcode:-4]@exclude#tag1#tag2

Generates a report for t-shirt ordered by the last 4 characters of the
barcode and doesn't display ``tag1`` and ``tag2``.

Tag or Patterns
~~~~~~~~~~~~~~~

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
