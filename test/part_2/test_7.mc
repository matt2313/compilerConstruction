string main()
{
    string out := "";
    int count := 10;
    
    do
    {
        // This is a really hacky way to get even numbers
        if((!count + 1) / 2 == (!count / 2))
        {
            out := !out ^ (!count as string) ^ "_";
        };
        
        count := !count - 1;
    }
    while(!count >= 0);
    
    return !out; // "10 8 6 4 2 0 "
}