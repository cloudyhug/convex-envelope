#include <stdlib.h>
#include <stdio.h>
#include <time.h>

/* random floats are in [-NMAX ; NMAX] */
#define N 10.0

float getrandomfloat(int sup)
{
    return (float)rand() / ((float)RAND_MAX / sup);
}

int main(void)
{
    int i, n;

    srand(time(NULL));

    n = 1;
    printf("Number of points to generate : ");
    scanf("%d", &n);

    printf("[");
    for (i = 0; i < n; ++i)
        printf("(%.2f,%.2f);", getrandomfloat(N) - N / 2, getrandomfloat(N) - N / 2);
    printf("(0.0,0.0)]\n") ;

    return 0;
}