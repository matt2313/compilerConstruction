int main()
{
    int x := 3;
    int y := 0;
    
    y := 4;
    
    int z := !x + !y;
    
    return !z - 7;
}