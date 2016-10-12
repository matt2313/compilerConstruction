**[Index](index) / Variables**

# Variables
A variable is assigned with the following syntax:
```
type var_name := exp
```
where `type` is the name of a type, `var_name` is a unique name consisting of alphanumeric characters (but not starting with a number), and `exp` is an expression of type `type`.

The value of a variable can then be retrieved by typing its name:
```
int x = 5;
x * 10;
```
In this code, line 2 would evaluate to `50`.

The assignment operator (`:=`) returns the value that was assigned, so the following is also possible:
```
int x = int y = int z = 100;
(int w = 50) * 2;
```
Here, both lines will evaluate to `100`. `x`, `y`, `z` store `100` while `w` will store `50`.
The assignment operator (`:=`) has lower priority than any other operator. If there is ambiguity the operation on the **right** will evaluate first.

**IMPORTANT NOTE: Since the lexer doesn't store any information on what identifiers have been assigned, it will not notice an error if you try to access an unassigned variable**