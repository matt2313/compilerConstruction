int foo()
{
    return 50;
}

int bar()
{
    return 100;
}

int main()
{
    int x := foo();         // 50
    int y := bar();
    return !x + !y;
}







/*
int foo()
{
    int x := 5;
    return !x;
}

int bar()
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
    int x := foo();
    int y := 0;
    y := bar();
    return !x + !y;
}
*/










/*
int foo()
{
    return 10;
}

int bar()
{
    return 100;
}

int main()
{
    int x := 0;
    int y := foo();
    int z := bar();
    
    if(!y > !z)
    {
        x := !y;
    }
    else
    {
        x := !z;
    };
    
    int i := 0;
    int j := 0;
    do
    {
        while(!j < 10)
        {
            x := !x + 1;
            j := !j + 1;
        };
        i := !i + 1;
    }
    while(!i < 10);
    
    return !x - 200;
}
*/