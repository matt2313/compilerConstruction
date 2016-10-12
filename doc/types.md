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