**[Index](index) / Types**

# Types
MattC supports the following types:
* Int
* Float
* Bool
* String

## Int
An `int` represents a single signed integer.
To create an `int` literal, simply write a number in base 10 with no decimal place. A `-` can be added to the start of the number to make it negative. For example, the following lines all create int literals:
```
1;
13;
10345;
-15;
-12;
```

## Float
A `float` represents a single floating point number.
To create a `float` literal, simply write a number in base 10 with a decimal place, or add an `f` to the end. A `-` can be added to the start of the number to make it negative. For example, the following lines all create float literals:
```
1.0;
13f;
10345.;
-15.0;
-12.5;
```

## Bool
A `bool` represents a single `true` or `false` value.
`bool` literals can be created by writing the keywords `true` or `false` in lower case.
```
true;
false;
```

## String
A `string` represents a series of characters. To create a `string`, surround a section of text in double quote marks `"`. For example:
```
"example_string";
"";
"Hello World!";
```

# Type Conversions

## Explicit Type Conversions
To explicitly cast an expression, use the `to` keyword like so:
```
exp to type_name
```
where `exp` is the expression that will be cast and `type_name` is the name of the type to cast it rto. For example:
```
3 to float; // 3.0
3.14 to string; // "3.14"
"10" to int; // 10
```

## Implicit Type Conversions
If one side of an operation is type `float` and the other is type `int`, the `int` will automatically be converted to a `float`.
```
1 + 0.5; // 1.5
3 + 8.0; // 11.0

1 > 0.5; // true
5.0 / 2 == 2.5; //  true
```

## Type Conversion Behaviour
* `int` to `float`: No loss of data
* `float` to `int`: The decimal part of the `float` will be truncated (so `1.9` becomes `1`, not `2`)
* `bool` to `int`/`float`: `true` becomes `1`/`1.0` and `false` becomes `0`/`0.0`
* `int`/`float` to `bool`: `0`/`0.0` becomes `false`, anything else becomes `true`
* Any type to `string`: The string will hold the data in the same way that it is written in the syntax of the language
* `string` to any other type: The `string` must contain a valid representation of an `int`, `float`, or `bool` otherwise an error with be thrown. Note that this means `bool` values must be lower case