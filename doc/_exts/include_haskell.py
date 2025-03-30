import os
from sphinx.directives.other import Include

class IncludeHaskell(Include):
    def run(self):
        """Transforms X.Y::tag into X/Y.hs before passing to the include directive."""
        if not self.arguments:
            raise self.error("Missing argument. Use 'Module.Name::tag'.")

        # Parse "X.Y::tag" into ("X/Y.hs", "tag")
        raw_arg = self.arguments[0]
        if "::" not in raw_arg:
            raise self.error("Invalid format. Use 'Module.Name::tag'.")

        module_path, tag = raw_arg.split("::", 1)

        # Convert "X.Y" into "X/Y.hs"
        self.arguments[0] = f"../../src/WarehousePlanner/{module_path.replace('.', '/')}.hs"

        # Automatically set start-after and end-before
        self.options.setdefault("start-after", f"rST::{tag}")
        self.options.setdefault("end-before", "-}")

        return super().run()

def setup(app):
    """Registers the custom directive."""
    app.add_directive("ihaskell", IncludeHaskell)
