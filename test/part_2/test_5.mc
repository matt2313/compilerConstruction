string main()
{
    print_string("Enter int:");
    int a := read_int();
    print_string("Enter float:");
    float b := read_float();
    float c := a * b;
    
    print_string(a as string ^ " X " ^ b as string ^ " = " ^ c as string);
        
    return c;
}