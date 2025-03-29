.. _reports:

Reports
~~~~~~~

Mop export
----------

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
--------------

A generic report can generated using special tags. All boxes will be
grouped first using the special tag *``report``*\ ``-group`` and then
*``report``*\ ``-key``. For each group (having the same key), a line
will be displayed with the content of *``report``*\ ``-group`` (for the
first level of grouping) and the *``report``*\ ``-value`` property (for
boxes grouped by *``report``*\ ``-key``). The ``report`` prefix can be
changed by setting an alternative prefix in the parameter field. This
allows multiples report to be defined within the same plan. The
following group attributes will be expanded :

.. include:: ../../src/WarehousePlanner/Report.hs
   :start-after: :REPORT EXPANSION:
   :end-before: -}

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
------------

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
---------------

Some reports like ``GenerateMovesWithTags`` accept a box selector as a
parameter. If an order is given, see (selection order), the specified
order of the boxes will be used. Also box tags shown in the report can
be filtered using ``@exclude`` and ``@include``. For example

::

   T-Shirt^^^[barcode:-4]@exclude#tag1#tag2

Generates a report for t-shirt ordered by the last 4 characters of the
barcode and doesn't display ``tag1`` and ``tag2``.

