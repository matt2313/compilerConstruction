// Tests scope inside loops

int main()
{
    int i := -1;
    int j := -1;

    new int i := 20 in
    while(i > 0)
    {
        print_int(i);
        print_string(" ");
        i := i - 1;
    }
    
    print_string("| ");
    
    new int i := 15 in
    new int j := 0 in
    while(j < i)
    {
        print_int(j);
        print_string(" ");
        j := j + 1;
    }
    
    return 0;
}