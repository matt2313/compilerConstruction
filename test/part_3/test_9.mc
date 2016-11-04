string intAsString(int toConvert)
{
    string ret := "";
    
    if(!toConvert == 0)
    {
        ret := "";
    }
    else if(!toConvert == 1)
    {
        ret := "one";
    }
    else if(!toConvert == 2)
    {
        ret := "two";
    }
    else if(!toConvert == 3)
    {
        ret := "three";
    }
    else if(!toConvert == 4)
    {
        ret := "four";
    }
    else if(!toConvert == 5)
    {
        ret := "five";
    }
    else if(!toConvert == 6)
    {
        ret := "six";
    }
    else if(!toConvert == 7)
    {
        ret := "seven";
    }
    else if(!toConvert == 8)
    {
        ret := "eight";
    }
    else if(!toConvert == 9)
    {
        ret := "nine";
    }
    else if(!toConvert == 10)
    {
        ret := "ten";
    }
    else if(!toConvert == 11)
    {
        ret := "eleven";
    }
    else if(!toConvert == 12)
    {
        ret := "twelve";
    }
    else if(!toConvert == 13)
    {
        ret := "thirteen";
    }
    else if(!toConvert == 15)
    {
        ret := "fifteen";
    }
    else if(!toConvert < 20)
    {
        ret := intAsString(!toConvert - 10) ^ "-teen";
    }
    else if(!toConvert < 100)
    {
        let int tens := !toConvert / 10 in
        let int units := !toConvert - (!tens * 10) in
        if(!tens == 2)
        {
            ret := "twenty-" ^ intAsString(!units);
        }
        else if(!tens == 3)
        {
            ret := "thirty-" ^ intAsString(!units);
        }
        else if(!tens == 5)
        {
            ret := "fifty-" ^ intAsString(!units);
        }
        else
        {
            ret := intAsString(!tens) ^ "ty-" ^ intAsString(!units);
        };
    }
    else
    {
        let int hundreds := !toConvert / 100 in
        let int tensAndUnits := !toConvert - (!hundreds * 100) in
        if(!tensAndUnits == 0)
        {
            ret := intAsString(!hundreds) ^ "-hundred";
        }
        else
        {
            ret := intAsString(!hundreds) ^ "-hundred-and-" ^ intAsString(!tensAndUnits);
        };
    };
    
    return !ret;
}

new string final := "" in
string main()
{
    new int curr := 1 in
    new int last := 150 in
    do
    {
        final := !final ^ intAsString(!curr) ^ ", ";
    }
    while((curr := !curr + 1) <= !last);
    
    return !final; // Should return a list of the numbers 1 to 150 in words
}