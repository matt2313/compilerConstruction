bool function()
{
    return false;
}

int main()
{
    int x := 10 + function_();
    return !x;
}