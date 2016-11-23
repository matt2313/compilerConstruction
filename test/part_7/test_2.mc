int main()
{
    int x := 0;
    
    int i := 0;
    while(!i < 5)
    {
        x := !x + 10;
        i := !i + 1;
    };
    
    int j := 6;
    do
    {
        x := !x + 100;
        j := !j - 1;
    }
    while(!j > 0);
    
    int k := 0;
    while(!k < 2)
    {
        int l := 0;
        do
        {
            x := !x + 1;
            l := !l + 1;
        }
        while(!l < 2);
        
        k := !k + 1;
    };
    
    return !x - 654; // 0
}