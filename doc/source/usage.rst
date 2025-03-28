Usage
============

Invoking WarehousePlanner
-------------------------

WarehousePlanner is invoked from the command line, like so:

    whp [option ...] file ...
    
    
Where each ``file`` is a filename containing whp instructions (.csv, .org or .whp) which will be executed in order.
The filename should be relative to the current directory unless a base directory is set using the ``-d`` option.
In that case each file will be relative to the base directory.

By default, ``whp`` opens a interactive explorer representing the warehouse. Pressing the ``<F1>`` key will list the main keybindings.

Warehouse Specification
------------------------

``whp`` works on a set of files constituting the warehouse specification. This specification is made of section of different type describing different aspect of the warehouse, such as shelves, boxes, layout, filling strategy etc ...

Each section can be written as a single file, where its extension determines the section type. Alternatively, multiple sections can be embedded in an orgmode_ file using orgmode `drawers <drawer_>`_.

Most sections are csv-like. In that case the file extension is ``.<section-type>.csv``. For other types the extension is just the `.<section-type>`. In both case, when embedded in orgmode document, the drawer name is the section type (followed by optional parameter).


For example the ``boxes`` section can be written as a  ``.boxes.csv`` file or in a ``:BOXES:`` drawer.
A ``wpl`` section (which is not a csv) can be written as a ``.wpl`` file or in a ``:WPL:`` drawer.

.. _csv: https://wikipedia.org/wiki/Comma-separated_values
.. _orgmode: https://orgmode.org/
.. _drawer: https://orgmode.org/manual/Drawers.html

Packing and Including Files
'''''''''''''''''''''''''''

An orgmode_ file can be use to "pack" different whp files into one using orgmode `drawers <drawer_>`_. This is the main way to include different sections in one file.

Each drawer corresponds to a section. The name of the drawer determines the section type and optional parameters.
Section type are parameters are separated by ``_``. For examples ``:BOXES_coming:`` describe a ``boxes`` section with one parameter ``coming`` [#coming]_

Apart from those special drawers it, is a normal orgmode document and all orgmode features can be used to document and/or structure the warehouse specifications.


Files can also be reused between different warehouse specifications using the `Import` section.

.. todo:: link to import section
.. [#coming] In that case, it will add the ``coming`` tag to all the created boxes.

Section Types
'''''''''''''

The following section types are available:

- shelves related

  - Shelves: create or update shelves
  - ShelfTags: modify shelf tags
  - ShelfSplit: split shelves into subshelves
  - ShelfJoin: opposite of split
  - UpdateShelves: modify shelves

- boxes related

  - Stocktake: create new boxes with location
  - Boxes: create new boxes
  - Moves: moves boxes 
  - Tags: modify boxes tags
  - MovesAndTags: move and tags simultaneously
  - Clones: create new boxes by cloning existing one
  - Deletes: delete existing boxes
  - Rearrange: rearrange boxes order
  - FreezeOrder: change internal box order
  - TransformTags: apply regexp transformation on tags

- Miscellaenous

  - Layout: describe shelf bays  and runs
  - Orientations: box orientation rules
  - Import: include local files
  - ColourMap: rgb colour description
  - CheckShelves: Check box overlap or out of box status
  - WPL: Warehouse Programming Languge.

Outputs
-------

On top the interactive explorer, ``whp`` can produce text outputs to generate miscellaneous reports, "freeze" the content of a warehouse and even generate high quality image (``.png``).

.. todo:: add reference to whp --help

Core concepts
-------------

WarehousePlanner deals with various concepts that will be reused throughout this document. Understanding these foundational ideas will help in comprehending the sections that follow.  


Physical objects
'''''''''''''''''

.. _box:
.. _shelf:

The two main concepts are **boxes** and **shelves**. 
A box must be in a shelf and as shelf can hold zero or many boxes.
Both have a name and dimensions (length, width, height) and can be **tagged**.

.. _bay:
.. _run:

Shelves which are stacked together form a **bay**.
A set of bays next to each other form a **run**.

Tags
''''

Tags can be used to select boxes and shelves but also alter their intrisinc properties.
For example, tagging a box with ``bg=green`` will display the box in green.
They can also been used to make complex selection or control the output of report generation.

.. _selector:

Selection
'''''''''

Selections play an important role in Warehouse Planner. Pretty much every action (like moving a box to a shelf) don't act on a single box or shelf but on a selection of boxes and a selection of shelves.
A selector provides a way to select multiple boxes or shelves using patterns.  

For example:  

- ``A*`` selects all boxes or shelves which name starts with ``A``.  
- ``#top`` selects all boxes or shelves tagged with ``top``.  
- ``A*#top`` selects all boxes or shelves whose names start with ``A`` and are tagged with ``top``.  
- ``A* to> #top`` moves all the boxes which name starts with ``A`` to the shelves tagged ``top``.
