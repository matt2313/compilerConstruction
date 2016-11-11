int main()
{
    int x := 3;
    int y := 0;
    
    y := 4;
    x := !x * !y;
    
    int ret := !x + 3;
    
    return !ret; // 15
}