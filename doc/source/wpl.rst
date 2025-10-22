.. _wpl:

WPL
===

Warehouse Programming Language
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Boxes and shelves can be manipulated via **WPL** a mini programming language.
Possible actions includes moving and tagging boxes, tagging and resizing shelves etc ...
Most actions are available through :ref:`section <section-types>`. Using WPL allows however more flexibility such as context, conditional and easier syntax.

Version
~~~~~~~
**WPL** exists in two versions:

- ``version:1`` uses identation to structure. It leads to parsing ambiguity and has been deprecated.
- ``version:2`` uses ``{...}`` to structure code. This is the new version which should be used.

Some code can be parsed by the two version with a different semantic. The specific version can be forced by starting a file with `version:<n>`.


Structure
~~~~~~~~~

.. ihaskell:: WPL.Parser::wpl-structure

Context
~~~~~~~
.. ihaskell:: WPL.ExContext::context

Condition
~~~~~~~~~
.. ihaskell:: WPL.Parser::wpl-condition

Commands
~~~~~~~~
.. ihaskell:: WPL.Parser::wpl-commands


