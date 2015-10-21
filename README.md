# escriptize

Replacement for `rebar escriptize`.

Important parts:
- By default, escript doesn't have a code path set, so you need to either put the beam files in the root of the ZIP file, or you need to include an escript directive.
- rebar puts the main application in its own subdirectory.
