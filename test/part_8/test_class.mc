/*
 * Printing isn't supported when compiled, but the program will still run without crashing
 */

int divisibleBy(int target, int test)
{
    int ret := -2;
    
    do
    {
        target := !target - !test;
        if(!target < 0)
        {
            ret := -1;
        }
        else if(!target == 0)
        {
            ret := 0;
        };
    }
    while(!target > 0);
    
    return !ret;
}

int main()
{
    int numCases := 100;
    
    int i := 0;
    int divBy3 := -3;
    int divBy5 := -3;
    while(!i < !numCases)
    {   
        i := !i + 1;
        divBy3 := divisibleBy(!i, 3);
        divBy5 := divisibleBy(!i, 5);

        if(!divBy3 == 0)
        {
            if(!divBy5 == 0)
            {
                print_string("FizzBuzz ");
            }
            else
            {
                print_string("Fizz ");
            };
        }
        else if(!divBy5 == 0)
        {
            print_string("Buzz ");
        }
        else
        {
            print_int(!i);
            print_string(" ");
        };
        
    };
    return 0;
}
