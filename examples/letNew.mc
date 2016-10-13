/* This file shows how to use let and new syntax */

int main()
{
    new bool testBool := false in
    testBool;
    
    let float myFloat := 2.5 in
    myFloat * 10;
    
    let string myString := "Hello World!" in
    print_string(myString as string);
    
    let int ret := 0 in
    return ret;
    
    let int a := 1 in
    let int b := 2 in
    let int c := 3 in
    a + b + c;
    
    new float d := 10 in
    new float e := 20 in
    new float f := 30 in
    d + e + f;
    
    let bool g := true in
    new bool h := false in
    //g OR h;
    g + h;
    
    let string i := "Page: " in
    new int j := 10 in
    let string k := ", Line: " in
    new int l := 15 in
    //i ^ (j as string) ^ k ^ (l as string);
    i + j - k * l;
}

let int x := 5 in
let string y := "test" in
new bool z := true in
float foo()
{
    return 3.14;
}