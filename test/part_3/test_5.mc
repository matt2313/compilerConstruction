// More testing on if statements and loops

int main()
{
    new int x := 0 in
    new int y := 10 in
    while((int diff := y - x) > 0)
    {
        print_string(diff as string ^ " ");
        y := y - 1;
    }
    
    print_string("| ");
    
    new int i := 25 in
    while((i := i - 1) >= 0)
    {
        print_string(i as string ^ " ");
    }
    
    int a := 12;
    int b := 25;
    int c := 42;
    int d := -1;
    if((int sum := a + b + c) > 200) // false
    {
        d := 200;
    }
    else if (sum > 100) // false
    {
        d := 100;
    }
    else if (sum > 50) // true
    {
        d := 50;
    }
    else
    {
        d := 0;
    }
    
    return d; // 50
}