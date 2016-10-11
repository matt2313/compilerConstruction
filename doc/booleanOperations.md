**[Index](index) / Boolean Operations**

# Boolean Operations

All boolean operations can be performed on `bool`s, including the result of other `bool` expressions.

## And, NAnd
The `AND` operator returns the `true` if both its operands are `true`, and false otherwise. It is an infix operator, so it is placed in between its operands.
`NAND` is the inverse of `AND`.
```
true AND true; // true
false AND true; // false

true NAND true; // false
false NAND true; // true
```

## Or, NOr
The `OR` operator returns the `true` if either (or both) of its operands are `true`, and `false` otherwise. It is an infix operator, so it is placed in between its operands.
`NOR` is the inverse of `OR`.
```
true OR false; // true
true OR true; // true
false OR false; // false

true NOR false; // false
true NOR true; // false
false NOR false; // true
```

## XOr, NXOr
The `XOR` operator returns the `true` if only one of its operands is `true`, and `false` otherwise. It is an infix operator, so it is placed in between its operands.
`NXOR` is the inverse of `XOR`.
```
true XOR false; // true
true XOR true; // false
false XOR false; // false

true NXOR false; // false
true NXOR true; // true
false NXOR false; // true
```

## Not
The `NOT` operator inverts its operand, so `true` becomes `false` and `false` becomes `true`
```
NOT false; // true
NOT true; // false
```

# Order of Precedence
Boolean operations have the following precedence:
1. Not
1. NXor
1. Nor
1. XOr
1. Or
1. NAnd
1. And

If there is any syntactic ambiguity, the operation on the **left** will be calculated first.