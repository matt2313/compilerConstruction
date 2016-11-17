int main()
{
    int x0 := 1;
    int x1 := 1;
    int x2 := !x0 + !x1;
    int x3 := !x1 + !x2;
    int x4 := !x2 + !x3;
    int x5 := !x3 + !x4;
    int x6 := !x4 + !x5;
    int x7 := !x5 + !x6;
    int x8 := !x6 + !x7;
    int x9 := !x7 + !x8;
    int x10 := !x8 + !x9;
    int y := 11;
    
    int z := !x10 + !y;
    
    return !z - 100;
}