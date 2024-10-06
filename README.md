# gaja

`gaja` is a toy Stack based VM implementation in Common Lisp.

This project has three main pieces, as follows:
1. `gaja` is the VM interpreter which runs `.gaja` binary files.
2. `hasti` is a compiler which creates the `.gaja` binary files from `.hasti` AST S-expressions.
3. `naga` is a reference language and parser which converts a high level `.naga` file into `.hasti` expressions.

## Pre-requisites

1. Common Lisp. Currently only works with `sbcl` for the Gaja VM and the Hasti Compiler.
2. Lua and Treesitter for the Naga transpiler.

## Running

Install it:
```
make
```
This creates all binaries in the `bin/` directory.

Setup shell to find the binaries:
```
source env.sh
```

Now, you can run the tools. Some sample `naga` files can be found in the `samples/` folder.
```
naga ./samples/ex1.naga out.hasti
hasti ./out.hasti out.gaja
gaja out.gaja
```
