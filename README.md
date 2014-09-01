Haskell-JSON-Patch
==================

JSON Patch Implementation in Haskell.

Probably best to install with cabal, though if you've got aeson and lens installed then it'll load into ghci
There's also a command line interface, it takes two arguments - a patch file and an input file, applies the patch to the input, and prints the patched version to stdout
