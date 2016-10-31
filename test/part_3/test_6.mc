// Tests basic function calls

int add(int x, int y)
{
    return !x + !y;
}

int main()
{
    return add(add(5, 5), add(3, add(2, -1))); // 14
}