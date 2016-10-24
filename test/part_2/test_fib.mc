int main()
{
    // Any more than this and integer overflow is a problem
    int i := 88;
    
    print_string("1, 1");
    int a := 1;
    int b := 1;
    int c := a + b;
    
    while(i > 0)
    {
        c := a + b;
        
        print_string(", " ^ (c as string));
        
        a := b;
        b := c;
        
        i := i - 1;
    }
    
    return c;
}