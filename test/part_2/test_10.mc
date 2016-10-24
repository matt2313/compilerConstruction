int main()
{
    int x := 1;
    int count := 8;
    
    while(count > 0)
    {
        x := x * 2;
        count := count - 1;
    }
    
    x := x * 10;
    x := x + 5;
    
    return x; // 2565
}