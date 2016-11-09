int main()
{
    1;
    2 + 3;
    4 + 5 + 6;
    
    // This breaks if the registers aren't set up correctly
    ((1 + 1) + (1 + 1)) + ((1 + 1) + (1 + 1)); // 8
    
    // This breaks if the compiler doesn't start allocating to the stack when it runs out of registers
    1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15; // 120
    
    return 4;
}