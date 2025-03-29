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
