**[Index](index) / Variables**

# Variables

## Standard Variables
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

## New and Let Statements
Variables can also be declared by using `new` and `let` statements. For example:
```
let int x := 10 in
new int y := 5 in
int main()
{
    return x * y; // returns 50
}
```

`new` and `let` statements create variables in the same way as standard variable assignment:
```
int main()
{
    int a := 10;
    int b := 20;
    
    let int count = b - a in
    while(count > 0)
    {
        print_int(count);
        print_string(" ");
        a := a - 1;
    }
    
    return 0;
}
```

`new` and `let` statements have their own scope, so
```
int main()
{
    int x := 0;
    int y := 0;
    
    new int x := 10 in
    y := x;
    
    return x + y; // returns 10, x is still 0
}
```
is valid