// Tests variable shadowing

let int x:= 10 in
int foo()
{
    return x;
}

let int x := 20 in
int main()
{
    int x := 30;
    return x; // 30
}