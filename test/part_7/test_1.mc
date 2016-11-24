int main()
{
    int x := 0;
    
    if(!x == 1)
    {
        x := 20;
    }
    else
    {
        x := 30;
    };
    
    x := !x - 30;
    
    
    int y := 10;
    if(true)
    {
        return x;
    }
    else
    {
        return y;
    } := 20;
    
    x := !x - 20;
    
    
    
    if(!y <= 10)
    {
        x := !x + 1;    // 1
    };
    
    if(!y < 10)
    {
        x := !x + 1;    // 1
    };
    
    if(!y >= 10)
    {
        x := !x + 1;    // 2
    };
    
    if(!y > 10)
    {
        x := !x + 1;    // 2
    };
    
    if(!y < 100)
    {
        x := !x + 1;    // 3
    };
    
    if(!y > 1)
    {
        x := !x + 1;    // 4
    };
    
    if(!y != 9)
    {
        x := !x + 1;    // 5
    };
    
    
    
    x := !x - 5;        // 0
    
    return !x;
}