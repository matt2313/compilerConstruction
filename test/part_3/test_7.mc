// Tests recursion

string makeIs(int count)
{
    string returnVal := "ERROR";
    
    if(count <= 0)
    {
        returnVal := "";
    }
    else
    {
        returnVal := "i" ^ makeIs(count - 1);
    }
    
    return returnVal;
}

string main()
{
    return makeIs(5); // "iiiii"
}