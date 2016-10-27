**[Index](index) / Functions**

# Functions

## Declaring a Function
A function is assigned with the following syntax:
```
type func_name(arg_list)
{
    body
}
```
where `type` is the name of a type, `func_name` is a unique name consisting of alphanumeric characters (but not starting with a number), `arg_list` is a list of arguments, and `body` is a list of expressions.

A functions arguments are defined as a list of a type name followed by an identifier, for example the function:
```
int my_func(int a, bool b, float c)
{
    ...
}
```
has 3 arguments: an `int`, `bool`, and `float` named `a`, `b`, and `c` respectively.

A `function` will evaluate to the value given in a `return` statement, for example the function:
```
int function()
{
    return -1;
}
```
will evaluate to `-1`.
All functions must return exactly 1 value, and their return statement must be called last without being in the scope of another nested statement (such as a `while` or `let` statement)

Functions can be evaluated like so:
```
foo(); // function with no agruments
bar("Hello, World!"); // function with 1 string argument
foo2(12.0, 5.0); // function with 2 float arguments
```