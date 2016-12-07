// Returns x
int foo(int x)
{
    int ret := 0;

    int i := 0;
    while(!i < !x)
    {
        ret := !ret + 10;
        i := !i + 1;
    };
    
    return !ret;
}

// Returns x * y * z
int bar(int x, int y, int z)
{
    int ret := 0;
    
    int i := 0;
    int j := 0;
    do
    {
        j := 0;
        while(!j < !y)
        {
            ret := !ret + !z;
            j := !j + 1;
        };
        
        i := !i + 1;
    }
    while(!i < !x);
    
    return !ret;
}

int main()
{
    int x := 0;
    int y := foo(10);
    int z := bar(20, 5, 4);
    
    if(!y > !z)
    {
        x := !y;
    }
    else
    {
        x := !z;
    };
    
    return !x - 400;
}