**[Index](index) / String Operations**

# String Operations

## Concatenation
The `^` operator returns a `string` consisting of the caracters from its left operand followed by the caracters from its right operand. It is an infix operator, so it is placed in between its operands.
```
"hello " ^ "world"; // hello world
"test" ^ ""; // test
"this " ^ "can " ^ "be " ^ "chained " ^ "together." // this can be chained together.
```

## Substring
The `substring` operator has the following syntax:
```
substring(str, start, len);
```
where `str` is the string to reduce, `start` is the index of the first char of the new `string` (starting at `0`), and `len` is the length of the new `string`.
For example:
```
substring("Hello World!", 1, 1); // "e"
substring("test", 0, 4); // "test"
substring("string_a_" ^ "string_b", 4, 10); // "ng_a_strin"
```

## Length
The length of a string can be obtained with the following syntax:
```
length(str);
```
where `str` is the string to measure. For example:
```
length("test"); // 4
length(""); // 0
length("Hello " ^ "World!"); // 12
```