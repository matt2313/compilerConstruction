// Tests let statements on if statements, and scope with if statements

int main()
{
    int a := 10;
    int b := 5;
    int c := 2;
    int d := -1;
    int e := -1;
    
    let int c := a * b * c in
    if(c == 100) // true
    {
        d := c;
    }
    else
    {
        d := 0;
    }
    
    if(c == 50) // false
    {
        int f := 50;
        e := f;
    }
    else if(c > 50) // false
    {
        int f := 45;
        e := f;
    }
    else
    {
        int f := 55;
        
        if(f / 5 == 11)
        {
            int e := 10;
            f := f - 10;
        }
        
        e := f;
    }
    
    return c * d + e; // 245
}