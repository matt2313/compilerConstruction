string main()
{
    string a := "hello_";
    string b := "world";
    string c := "!";
    
    return !a ^ !b ^ !c; // 'hello world!'
}