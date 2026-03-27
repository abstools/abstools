
"""
Sphinx extension: eifilelink
Provides an :eifilelink: role and .. eifilelink:: directive
that expand to <a href="BASE_URL + path" target="_blank">title</a>
"""
import os
from pathlib import PurePosixPath
from urllib.parse import urlparse, urlencode

from docutils import nodes
from docutils.parsers.rst import Directive, directives


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _build_url(base: str, path: str, app: str | None = None) -> str:
    url = base.rstrip("") + path
    if app:
        url += f"&app={app}"
    return url


def _default_title(path: str) -> str:
    """Derive a display title from the path, like Hugo's path.Base."""
    return PurePosixPath(path).name


# ---------------------------------------------------------------------------
# Inline role  — simple one-liner usage:
#   :eifilelink:`/collaboratory/examples/Deadlock/BOL/uglyChain.abs`
#   :eifilelink:`My label </collaboratory/examples/Deadlock/BOL/uglyChain.abs>`
# ---------------------------------------------------------------------------

def eifilelink_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    base = inliner.document.settings.env.config.eifilelink_base

    # Split off optional "label <..." wrapper
    if "<" in text and text.endswith(">"):
        label, rest = text[:-1].rsplit("<", 1)
        label = label.strip()
        rest  = rest.strip()
    else:
        label = None
        rest  = text.strip()

    # Split path and optional app=foo
    path, _, app_param = rest.partition("|app=")
    path = path.strip()
    app  = app_param.strip() or None

    title = label or _default_title(path)
    url   = _build_url(base, path, app)

    node = nodes.reference(rawtext, title, refuri=url)
    node["classes"].append("eifilelink")
    return [node], []

# ---------------------------------------------------------------------------
# Directive  — richer usage with named options (mirrors Hugo named params):
#
#   .. eifilelink:: /collaboratory/examples/Deadlock/BOL/uglyChain.abs
#      :app: dsa
#      :title: uglyChain.abs          (optional — auto-derived if omitted)
# ---------------------------------------------------------------------------

class EifileLinkDirective(Directive):
    required_arguments = 1          # the path
    optional_arguments = 0
    option_spec = {
        "app":   directives.unchanged,
        "title": directives.unchanged,
    }
    has_content = False

    def run(self):
        env = self.state.document.settings.env
        base = env.config.eifilelink_base

        path  = self.arguments[0].strip()
        app   = self.options.get("app")
        title = self.options.get("title") or _default_title(path)
        url   = _build_url(base, path, app)

        node = nodes.reference(path, title, refuri=url)
        node["classes"].append("eifilelink")

        # Wrap in a paragraph so it renders cleanly in block context
        para = nodes.paragraph()
        para += node
        return [para]


# ---------------------------------------------------------------------------
# Sphinx wiring
# ---------------------------------------------------------------------------

def setup(app):
    app.add_config_value("eifilelink_base", "", "env")

    app.add_role("eifilelink", eifilelink_role)
    app.add_directive("eifilelink", EifileLinkDirective)

    return {
        "version": "0.1",
        "parallel_read_safe": True,
    }
