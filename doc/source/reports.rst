.. _reports:

Reports
~~~~~~~

.. ihaskell:: Report::mop

Generic report
--------------
.. ihaskell:: Report::generic

Best reports
------------

.. ihaskell:: Report::bests

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

