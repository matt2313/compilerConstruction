int foo(int x)
{
    return !x;
}

int bar(int y)
{
    int i := 0;
    
    while(!i < 15)
    {
        i := !i + 1;
    };
    
    return !i;
}

int main()
{
    int x := foo(5);
    int y := 0;
    y := bar(15);
    return !x + !y;
}