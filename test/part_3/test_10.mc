bool checkPrime(int x)
{
    bool ret := true;
    
    if(!x < 2)
    {
        ret := false;
    }
    else if (!x == 2)
    {
        ret := true;
    }
    else
    {
        new float i := 2.0 in
        new bool break := false in
        while(NOT !break)
        {
            new float division_f := !x / !i in
            new int division_i := !x / (!i as int) in
            if(!division_f == !division_i)
            {
                ret := false;
                break := true;
            };
            
            i := !i + 1.0;
            
            if(!i >= !x)
            {
                break := true;
            };
        };
    };
    
    return !ret;
}

let int start := 2 in
new int max := -1 in
int main()
{
    /*
    while(!max < !start)
    {
        print_string("Enter an integer below 1000: ");
        max := read_int();
    };
    */
    max := 1000;
    
    print_string("primes up to " ^ (!max as string) ^ " are: ");
    
    new int i := !start in
    while(!i <= !max)
    {
        if(checkPrime(!i))
        {
            print_int(!i);
            print_string(", ");
        };
        
        i := !i + 1;
    };
    
    return !max; // Returns all primes below the given number
}