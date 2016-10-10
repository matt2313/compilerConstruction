# Compiler Construction
Repository for the source files and documentation of the MattC compiler, created for the continuous assesment of the Compiler Construction module.


## Contents

- An `/srs/` folder that holds the source files for the compiler (`mattc_par.mly`, `mattc_lex.mly`, `mattc.ml`), as well as scripts to comile the compiler (`build_mattc`, `build_mattc.bat`)
- An `/examples/` folder containing test programs that demonstrate code written in MattC
- A `/doc/` folder containing documentation on how to write MattC code


## How to run the compiler

1. Navigate to the `/src/` folder
2. Run `build_mc` (for unix) or `build_mc.bat` (for windows)
  - On the lab machines, remember to run the command `module load opam` so that the ocaml binaries are available
3. You can then run `build_mc <filename>`, where `<filename>` is the file you want to compile. MattC files use the `.mc` file extension
