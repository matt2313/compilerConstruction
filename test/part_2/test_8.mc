int main()
{
    int a := 25;
    int b := !a - 1;
    int c := !b / 2;
    int d := !c / 4;
    
    return !d; // 3
}