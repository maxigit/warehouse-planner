Boxes and shelves selector
~~~~~~~~~~~~~~~~~~~~~~~~~~

Most operations, (moving and tagging) operates on boxes. To do so, a set
of boxes is selected and then the operation is applied. The main way of
selecting a box is by specifying a style, which will select all boxes of
the given style. However, more complex selection can be selecting boxes
by tags and locations (as well as location tag). The format of a box
selector is the following

::

   style pattern # tag(s) pattern / location pattern # location tag

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

.. _number:

number restriction
------------------

.. ihaskell:: Selector::number-selector

.. _priority:

Priority and selection order
----------------------------
.. ihaskell :: Move::priority

Set and use reference
---------------------

.. ihaskell :: Csv::with-previous
