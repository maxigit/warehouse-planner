from sphinx.highlighting import lexers
from pygments.lexer import RegexLexer
from pygments.token import Comment, Text, Name, String, Number

import os
import sys
import glob

# Add the _exts directory to the Python path
sys.path.append(os.path.abspath("../_exts"))

# Automatically load all Python files in _exts
extensions = [
    os.path.splitext(os.path.basename(f))[0]
    for f in glob.glob(os.path.join("../_exts", "*.py"))
]

class CommentOnlyLexer(RegexLexer):
    """A lexer that only highlights comments (starting with --) and leaves everything else plain."""
    name = "CommentOnly"
    aliases = ["commentonly"]
    filenames = []
    tokens = {
           "root": [
                       (r"--.*$", Comment),    # Match '--' and everything after it (comments)
                       (r"#-?\w+", Name.Tag),    # Match '#' followed by word characters (tags)
                       (r"\$\{[^}]+\}", String.Interpol),  # Match '${...}' (interpolated expressions)
                       (r"\$\[[^\]]+\]", String.Interpol),  # Match '$[...]' (another interpolated expression)
                       (r"\{[^}]+\}", Name.Namespace),  # Match '{...}' (expressions)
                       (r"\[[^}]+\]", Name.Variable),  # Match '[...]' (range)
                       (r".", Text),           # Everything else is plain text
                   ]
    }

# Register the lexer
lexers["commentonly"] = CommentOnlyLexer()

# Set it as the default language for code blocks
highlight_language = "commentonly"

# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'WarehousePlanner'
copyright = '2025, Maxime Bourget'
author = 'Maxime Bourget'
release = '2.2.0'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions.append('sphinx.ext.todo')

templates_path = ['_templates']
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']

# -- Options for Todo --------------------------------------------------------
#todo_include_todos = True

# -- Options for Read the doc theme  --------------------------------------------------------

html_theme_options = {
     'logo_only': False,
    'prev_next_buttons_location': 'bottom',
    'style_external_links': False,
    'vcs_pageview_mode': '',
    'style_nav_header_background': 'purple',
    #'flyout_display': 'hidden',
    #'version_selector': True,
    #language_selector': True,
    # Toc options
    'collapse_navigation': True,
    'sticky_navigation': True,
    'navigation_depth': 4,
    'includehidden': True,
    'titles_only': False
}


