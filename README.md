# Compiler Construction

Repository for the source files and documentation of the MattC compiler, created for the continuous assesment of the Compiler Construction module.


## Contents

- An `/srs/` folder that holds the source files for the compiler, as well as scripts a makefile compile the compiler
- A `/bin` folder containing the compiled compiler
- A `/test/` folder containing test programs that demonstrate code written in MattC
- A `/doc/` folder containing documentation on how to write MattC code


## How to Run the Compiler

1. Navigate to the `/src/` folder
2. Run `make` (for Ubuntu) or `make windows` (for Windows)
  - On the lab machines, remember to run the command `module load opam` so that the ocaml binaries are available
3. You can then run `mattc <filename>`, where `<filename>` is the file you want to compile. MattC files use the `.mc` file extension
  - The compiler will work on any number of files given as arguments
  - The `-v` option prints the full parse tree
  - The `-e` option will evaluate the file after it has been parsed, and display the return value of the main function.
  - The `-i` option specifies a file to use as input for read functions wile evaluating
  - The `-terminalInput` option sets the input to come directly from the terminal. (This is the default setting)
  - The `-o` option optimises the code before evaluation and compilation
  - The `-no` option stops the code from being optimised. (This is the default setting)
  - The `-ov` option prints the optimised parse tree, has no effect unless `-o` is also set
  - The `-q` option clears the `-v` and `-ov` options
  - The `-c` option actually compiles the code
  - The `-out` option sets the filename of the compiled file
  - The `-s` option simulates the generated machine instructions and shows the return value
  - The `-exp` option is used for testing, and checks that the evaluated or simulated program matches the given input


## Syntax in a Nutshell

Most of the syntax is C-like, implementing the features of the syntax tree given. Notable differences are:
* Assignment is done with `:=` instead of `=`
* Dereferencing variables is done by using the `!` operator
* Boolean operations use plain text in upper case (`AND`, `OR`, `NOT`, etc.)
* Four types are supported: `int`, `float`, `bool`, and `string`
* Type conversion is done with the keyword `to`, e.g. `50 to string` gives the string `"50"`
* There is implicit type conversion from `int` to `float`
* String concatenation is done with `^`
* `length(str)` and `substring(str, start, len)` functions are built in to the language
* All functions must return a value, there is no `void` type
* I/O operations are performed using the functions `read_int()`, `print_int()`, `read_string()`, etc.
* `let ... in` and `new ... in` statements are supported

For complete documentation on the syntax, view `/doc/index.md` or look at the example code provided


## Test Code

Test code can be found in the `/test/` directory. There is a script to run all the tests cases at once, as well as a script for each part that will run all the test files for each part's respective folder.