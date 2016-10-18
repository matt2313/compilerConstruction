/* This code shows how to use strings  */

int main()
{
    "This is a string";
    "Hello " ^ "World!";
    
    substring("Hello World!", 2, 1); // "l"
    substring("test string", 0, 11); // "test string"
    substring("string_a_" ^ "string_b", 5, 10); // "g_a_string"
    
    length("test string"); // 11
    length(""); // 0
    length("Hello " ^ "World!!!"); // 14
    
    return length("test complete");
}