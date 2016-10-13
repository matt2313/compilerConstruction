/* This code shows how to use strings, get input, and print ouput  */

int main()
{
    read_int();
    print_int(0);
    print_int(17);
    print_int(12 + 17);
    print_int((60 / 2) - (30 / 2));
    100 * read_int();

    "This is a string";
    read_string();
    print_string("Hello World!");
    "Hello " ^ "World!";
    print_string("Hello " ^ "World!");
    
    substring("Hello World!", 2, 1); // "l"
    substring("test string", 0, 11); // "test string"
    substring("string_a_" ^ "string_b", 5, 10); // "g_a_string"
    
    length("test string"); // 11
    length(""); // 0
    length("Hello " ^ "World!!!"); // 14
    
    return length("test complete");
}