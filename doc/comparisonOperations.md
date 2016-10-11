**[Index](index) / Comparison Operations**

# Comparison Operations

All comparison operations can be performed on `int`s, including the result of other `int` expressions, and always return a `bool`.

## Equal and Not Equal
The `=` operator returns 'true' if boths its operands have the same value, and 'false' otherwise. It has an inverse operation, `!=`.
```
5 == 3 + 2; // true
2 == 1; // false
30 != 10 + 5 // true
17 != 15 + 2 // false
```

## Less Than, Greater Than, Less Than or Equal to, Greater Than or Equal to
The `<` operator returns `true` if the left operand has a strictly smaller value than the right operand, and returns `false` otherwise.
```
5 < 10; // true
5 < 0; // false
5 < 5; // false
```

The `>` operator returns `true` if the left operand has a strictly larger value than the right operand, and returns `false` otherwise.
```
100 > 99; // true
100 > 120; // false
100 > 100; // false
```

The `<=` operator returns `true` if the left operand has a smaller or equal value to the right operand, and returns `false` otherwise.
```
5 <= 10; // true
5 <= 0; // false
5 <= 5; // true
```

The `>=` operator returns `true` if the left operand has a greater or equal value to the right operand, and returns `false` otherwise.
```
100 >= 99; // true
100 >= 120; // false
100 >= 100; // true
```

Note that `<` and `>=`, as well as `>` and `<=`, are each others inverse operations.

# Order of Precedence
Comparison operations have the following precedence:
1. Equal
1. Not equal
1. Greater than or equal
1. Less than or equal
1. Greater than
1. Less than

If there is any syntactic ambiguity, the operation on the **left** will be calculated first.