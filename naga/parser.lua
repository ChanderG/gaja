#!/usr/bin/env lua
local lts = require("ltreesitter")

local so_dir = os.getenv("NAGA_BIN_DIR")
if so_dir == nil then
   so_dir = "."
end

local par = lts.load(so_dir .. "/parser.so", "naga")

if arg[1] == nil then
   print("Usage: naga <infile> [<outfile>]")
   print("Missing input file name! Unable to proceed.")
   os.exit()
end

if arg[2] == nil then
   outfile = "out.hasti"
else
   outfile = arg[2]
end

local f = assert(io.open(arg[1], "r"))
local code = f:read("*all")
f:close()

local tree = par:parse_string(code)
local out = assert(io.open(outfile, "w"))

function handle_node(node)
   out:write("(")

   n = node:name()
   if n == "ident" or n == "numeral" then
      out:write(n, string.sub(code, node:start_byte(), node:end_byte()))
   else
      -- maybe risky if there is use of _ as an operator
      n = string.gsub(n, "_", "-")
      out:write(n)
   end

   for child in node:named_children() do
      handle_node(child)
   end
   out:write(")")
end

handle_node(tree:root())
out:close()

io.write("Wrote ", outfile, " .\n")
