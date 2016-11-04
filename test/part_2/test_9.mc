int add(int a, int b)
{
    return !a + !b;
}

string main()
{
    print_string("Enter 2 strings:");
    string a := read_string();
    string b := read_string();
    string c := !a ^ !b;
    print_string(!c);
    
    return !c;
}