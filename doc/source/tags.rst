Working with Tags
~~~~~~~~~~~~~~~~~

Virtual tags
------------

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
`Properties`_) to the dimension.

Properties
----------

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

.. _styling:

Styling properties
''''''''''''''''''

.. ihaskell:: Styling::tags


.. _colours-1:

Colours
"""""""

.. ihaskell:: Styling::colour

Group functions
''''''''''''''''''''''''

.. ihaskell:: Base::group-function

Evaluation
''''''''''
.. ihaskell:: Base::evaluator

Breaks And Slots
----------------
.. ihaskell:: Base::break

Box attributes
--------------
.. ihaskell:: Base::box-attributes
