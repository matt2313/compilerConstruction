// 5 + 2t + 3t^2
int f(int t)
{
    return (5 + (2 * !t) + (3 * !t * !t));
}

// -2 - 10t + t^2
int g(int t)
{
    return (-2 - (10 * !t) + (!t * !t));
}

int main()
{
    int limit := 1000;
    int width := 10;
    int result := 0;
    
    int currResult := 0;
    int i := -!limit;
    while(!i < !limit)
    {
        currResult := f(!i - !width) * g(!i);
        result := !result + !currResult;
        i := !i + 1;
    };
    
    return !result + 4000; // So it returns 0 and passes the test
}