**[Index](index) / Numerical Operations**

# Numerical Operations

All numerical operations can be performed on `int` and `float` expressions.

## Addition
The `+` operator returns the value of its operands added together. It is an infix operator, so it is placed in between its operands.
```
1 + 2; // 3
1 + (3 * 5); // 16
1 + 2 + 3 + 4 + 5 + 6; // 21
```

## Subtraction
When used with 2 operands, the `-` operator returns the value of its left operand minus its right operand. It is an infix operator, so it is placed in between its operands.
```
12 - 2; // 10
5 - (10 - 2); // - 3
1 - 2 - 3 - 4 - 5 - 6; // -19
```
Note that if the the 1st operand is missing, it will be evaluated as a negation instead.

## Multiplication
The `*` operator returns the value of its 2 operands multiplied together. It is an infix operator, so it is placed in between its operands.
```
5 * 3; // 15
10 * (3 + 4) * 10; // 700
2 * 2 * 2 * 4; // 32
```

## Division
The `/` operator returns the value of its left operand divided by its right operand. Attempting to divide by `0` will produce a fatal error. It is an infix operator, so it is placed in between its operands.
```
200 / 4; // 50
(0 - 12) / 4; // -3
360 / 3 / 4 / 15 / 2; // 1
```

## Negation
When used with 1 operand, the `-` operator negates the value of its operand.
```
-5; // -5
- 10; // -10
-(5 + 3); // -8
```
Note that if there is an int expression to the left of the `-`, it will be evaluated as subtraction instead. Use brackets to avoid this.

# Order of Precedence
Numerical operations have the following precedence:

1. Negation
1. Division
1. Multiplication
1. Subtraction
1. Addition

(note that semantically multiplication and division have equal precedence, as do addition and subtraction)

If there is any syntactic ambiguity, the operation on the **left** will be calculated first.