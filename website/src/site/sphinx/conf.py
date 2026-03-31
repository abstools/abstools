# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

import sys
import os

# -- Project information -----------------------------------------------------

project = 'The ABS Language'
html_short_title = "ABS"
copyright = 'CC-BY-SA 3.0'
author = 'The ABS Development Team'

# -- General configuration ---------------------------------------------------

# Are we building the collaboratory:
collaboratory = os.environ.get("SPHINX_COLLABORATORY", "false").lower() == "true"
if collaboratory:
    eifilelink_base = "/ei/clients/web/index.html?file="
else:
    eifilelink_base = "http://ei.abs-models.org:8082/clients/web/index.html?file="

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
# extensions = [ 'sphinx.ext.mathjax', 'sphinxcontrib.mermaid' ]

sys.path.append(os.path.abspath("_extensions"))
extensions = [ 'sphinxcontrib.plantuml', 'eifilelink' ]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'
html_theme_options = {
    'body_text_align': 'justify',
    'description': 'A timed actor-based modeling language',
    # 'fixed_sidebar': True,
    'logo': 'ABS_logo_colors.png',
    # 'logo_name': True,
    'github_user': 'abstools',
    'github_repo': 'abstools',
    # 'github_banner': True,
    'github_button': True,
    'sidebar_collapse': True,
    # 'show_relbars': True,
}
html_static_path = ['_static']
html_baseurl = 'https://abs-models.org/'
# html_logo = '_static/ABS_logo_colors.png'
