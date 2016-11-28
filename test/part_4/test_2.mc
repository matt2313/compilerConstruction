let string sep := "," in
string main()
{
    float a := 3.1415 * 10. + (2. / 4.); // 31.915
    float b := 0.5 * 20; // 10.
    int c := 1 + 2 + 4 + 8 + 16 + 32 + length("test"); // 67
    bool d := (true OR false) AND (10 < 6); // false
    string e := "hello" ^ "_" ^ "world!"; // "hello world!"
    
    string final := (!a as string) ^ !sep ^ (!b as string) ^ !sep ^ (!c as string) ^ !sep ^ (!d as string) ^ !sep ^ !e;
    return !final; // "31.915, 10., 67, false, hello world!"
}