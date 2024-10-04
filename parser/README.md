# Parser

Parse .naga files to ast format, ie .kapi files.

## Prereq

1. `tree-sitter-cli` tool (with dependencies needed for it)
2. `lua` installation with `luarocks`.
3. Lua bindings for Tree sitter: `luarocks install ltreesitter`

## Run

Build the parser for `naga`:
```
make build
```

Run the parser for an input file:
```
./parser.lua ../samples/ex1.naga
```
will output the file `out.kapi`.
