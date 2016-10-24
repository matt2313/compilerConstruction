int add(int a, int b)
{
    return a + b;
}

string main()
{
    print_string("Enter 2 strings:");
    a := read_string();
    b := read_string();
    c := a ^ b;
    print_string(c);
    
    return c;
}