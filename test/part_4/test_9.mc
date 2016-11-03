int calc(int x, int y)
{
    return 2 * !x + 3 * !y;
}

int main()
{
    int x := calc(1, 1) + calc(2, 2) + calc(5, 10); // 55
    return !x;
}