int foo()
{
    return 50;
}

int main()
{
    int x := foo();         // 50
    return !x - 50;
}