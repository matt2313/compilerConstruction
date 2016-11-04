int fact(int n)
{
    return if(!n <= 1)
    {
        return 1;
    }
    else
    {
        return !n * fact(!n - 1);
    };
}

int main()
{
    int x := fact(3); // 6
    int y := fact(5); // 120
    int z := fact(read_int());
    
    return !x + !y + !z;
}