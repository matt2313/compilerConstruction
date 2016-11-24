int foo()
{
    /*
    int x := 300;
    x := !x - (5 * 50);
    return !x;
    */
    return 50;
}

/*
int bar(int x, int y, int z)
{
    return !x + !y + !z;
}
*/

int main()
{
    int x := foo();         // 50
    
    return !x - 50;
    
    //int y := bar(1, 2, 3);  // 6
    //return !x + !y - 56;    // 0
}