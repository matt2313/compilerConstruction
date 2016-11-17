#include <stdio.h>
#include <stdlib.h>

void print(int x)
{
    printf("%d\n", x);
    exit(0);
}

int main()
{
    int y = 260;
    print(y);
    return 1;
}