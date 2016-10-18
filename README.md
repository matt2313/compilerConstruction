# Compiler Construction

Repository for the source files and documentation of the MattC compiler, created for the continuous assesment of the Compiler Construction module.


## Contents

- An `/srs/` folder that holds the source files for the compiler (`mattc_par.mly`, `mattc_lex.mly`, `mattc.ml`), as well as scripts to compile the compiler (`build_mattc.sh`, `build_mattc.bat`)
- A `/test/` folder containing test programs that demonstrate code written in MattC
- A `/doc/` folder containing documentation on how to write MattC code


## How to Run the Compiler

1. Navigate to the `/src/` folder
2. Run `build_mc.sh` (for unix) or `build_mc.bat` (for windows)
  - On the lab machines, remember to run the command `module load opam` so that the ocaml binaries are available
3. You can then run `mattc <filename>`, where `<filename>` is the file you want to compile. MattC files use the `.mc` file extension
  - The compiler will work on any number of files given as arguments, and the `-v` option can be used to display the evaluation of each line as it is parsed. This is for debug purposes and does not work with some features, like variable evaluation


## Syntax in a Nutshell

Most of the syntax is C-like, implementing the features of the syntax tree given. Notable differences are:
* Assignment is done with `:=` instead of `=`
* Dereferencing variables is done by simply writing the name of the variable, there is no dereference operator
* Boolean operations use plain text in upper case (`AND`, `OR`, `NOT`, etc.)
* Four types are supported: `int`, `float`, `bool`, and `string`
* Type conversion is done with the keyword `to`, e.g. `50 to string` gives the string `"50"`
* There is implicit type conversion from `int` to `float`
* String concatenation is done with `^`
* `length(str)` and `substring(str, start, len)` functions are built in to the language
* All functions must return a value, there is no `void` type
* I/O operations are performed using the functions `read_int()`, `print_int()`, `read_string()`, etc.

For complete documentation on the syntax, view `/doc/index.md` or look at the example code provided


## Test Code

Test code can be found in the `/test/` directory. There is also a script for each part that will run all the test files in one go in each part's respective folder.