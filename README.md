# escriptize

Replacement for `rebar escriptize`.

Important parts:
- By default, escript doesn't have a code path set, so you need to either put the beam files in the root of the ZIP file, or you need to include an escript directive.
- rebar puts the main application in its own subdirectory. It apparently figures out the application name by looking in `src/*.app.src`.
- escript looks for `basename:main/1` unless you use `-escript main Mod`.

What parameters _do_ we have?

- Output file name.
- Base application name.
- What beams to include.

    ./escriptize -o foo -i 
