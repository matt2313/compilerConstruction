[Index](index) / Basic Syntax

# Basic Syntax

Every line in MattC must consist of any number of expressions, each followed by a semicolon (`;`). Whitespace and comments can be used to seperate keywords and identifiers, but are otherwise ignored.

## Whitespace
Any space (` `), tab (`\t`), or new line (`\n`) is treated as whitespace, and will be ignored by the compiler. The compiler will see
```
15 + 5;
20 - 9;
3 * 6;
```
the same as
```
15+5;20-9;3*6;
```
However, whitespace should still be used to keep code readable.

## Comments
MattC supports in-line and multi-line comments.
#### In-line comments:
Any characters on a line following from two slashes (`//`) will be ignored. For example, line 2 in the following code would be ignored:
```
5 + 5;
// The next line equals 8
10 - 2;
```
And every character on line 2 after the `;` in the following code would be ignored:
```
5 + 2;
10 * 2; // This will be 20
15 - 5;
```
Note that in both examples line 3 will still run.

#### Multi-line comments:
Any group of characters preceeded by `/*` and followed by `*/` will be ignored. You can still use `/` or `*` alone without causing errors or ending the comment. For example, line 2 in the following code would be ignored:
```
0 - 1;
/* Both of these lines give -1 */
5 - 6;
```
This style of comment can be used to comment out several lines of code in one go, in the following code lines 2, 3, 4, 5, and 6 are ignored:
```
23 * 6;
/*
  
 Both of these lines make 138
 
 */
180 - 42;
```
Multi-line comments can even be used in the middle of expressions. The following code will compile without error:
```
1 + /* Using 2 because 1 + 1 is boring */ 2;
```

## Bracket Operators
Any expression can be surrounded by `(` and `)` and still be valid. While this is useless on its own, it can be used to circumvent the normal precedence rules of the language. For example:
```
5 * (3 + 1); // Without brackets this would evaluate to 16 instead of 20
```