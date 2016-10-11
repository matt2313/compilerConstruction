**[Index](index) / Loops and Conditional Statements**

# Loops and Conditional Statements
Loops and conditional statements are used to alter the control flow of the program.
For all syntax listed here, brackets around the `condition` are recommended but not required.

## While Loop
A while loop will repeat a list of statements for as long as a given condition is met. The syntax for a while loop is:
```
while( condition )
{
    expressionList
}
```
Here, `expressionList` will be repeated as long as `condition` evaluates to `true`. In a while loop, the condition is evaluated before the loop starts, so it will run 0 times if the initial condition evaluates to false.

## Do While Loop
A while loop will repeat a list of statements for as long as a given condition is met. The syntax for a while loop is:
```
do
{
    expressionList
}
while( condition );
```
Here, `expressionList` will be repeated as long as `condition` evaluates to `true`. In a do while loop, the condition is evaluated after the body has executed, so it will always run at least 1 time.

## If Statement
An if statement will run different code if a condition is met. The syntax is:
```
if( condition )
{
    runIfTrue
}
runAlways
```
If `condition` evaluates to `true` then `runIfTrue` will execute. `runAlways` will always execute afterwards, regardless of `condition`.
If statements can be extended by adding an `else` block like so:
```
if( condition )
{
    runIfTrue
}
else
{
    runIfFalse
}
runAlways
```
If `condition` evaluates to `true` then `runIfTrue` will execute, if `condition` evaluates to `false` then `runIfFalse` will execute instead. `runAlways` will always execute afterwards, regardless of `condition`.

`else if` statements are not supported, use nested `if` statements instead.

## Nesting
All conditional statements can be nested within each other and still be valid, for example:
```
while(true)
{
    if(4 < 5)
    {
        while(true)
        {
            11 * 2;
        }
    }
    else
    {
        10 - 3;
    }
}
```