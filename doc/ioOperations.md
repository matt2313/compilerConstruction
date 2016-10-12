**[Index](index) / Input and Output Operations**

# Input and Output Operations

## Input
The commands `read_int()` and `read_string()` can be used to read an `int` or `string` from the terminal, respectively. For example:
```
read_int(); // Gets int from user
(read_int() + read_int()) * 10; // Adds to given ints together, then multiplies them by 10

read_string(); // Gets string from user
```

## Output
The commands `print_int()` and `print_string()` can be used to print an `int` or `string` to the terminal, respectively. For example:
```
print_int(10);
print_int(100 / 50); // 2

print_string(""); // Prints a blank line
print_string("Hello World!");
print_string("example " ^ "string"); // example string
```