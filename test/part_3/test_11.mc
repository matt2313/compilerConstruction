int main()
{
    int x := 0;
    
    let int a := 1 in
    let int b := 20 in
    let int c := 300 in
    new int d := 0 in
    {
        d := !d + !a;
        d := !d + !b;
        d := !d + !c;
        x := !d;
    }
    
    int y := 
    if(!x == 300) // false
    {
        return 0;
    }
    else if(!x == 320) // false
    {
        return 0;
    }
    else
    {
        return 4000;
    };
    
    int A := 0;
    int B := 0;
    if(!x + !y == 4321){ return A; } else { return B; } := 50000;
    
    int z :=
    if(!B == 0) // true
    {
        return 600000;
    }
    else
    {
        return 0;
    };
    
    int w :=
    if(!B > 0) // false
    {
        return 0;
    }
    else if(!B == 0) // true
    {
        return 7000000;
    }
    else
    {
        return 0;
    };
    
    return !x + !y + !A + !z + !w; // 7654321
}