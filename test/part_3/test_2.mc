// Tests let statements on if statements

int main()
{
    int a := 10;
    int b := 5;
    int c := 2;
    int d := 0;
    
    let int c := a * b * c in
    if(c == 100) // true
    {
        d := c;
    }
    else
    {
        d := 0;
    }
    
    return c * d; // 200
}