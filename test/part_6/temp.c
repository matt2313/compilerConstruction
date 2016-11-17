#include <stdlib.h>

int main()
{
    int y = 200;
    
    int x = 360;
    x = 720;
    x = 1480;
    x = x + x;
    x = x + 1;
    x = 1 + x;
    x = 100 + y;
    
    int* z = malloc(sizeof x);
    *z = 500;
    
    return x;
}