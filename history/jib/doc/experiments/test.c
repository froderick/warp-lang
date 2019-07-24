#include <stdio.h>

int foo(int blab) {
    printf("hi more: %i\n", blab);
    return 200;
}

typedef struct Blob {
    unsigned long long a, b, c, d, e, f, g, h, i, j, k;
} Blob;

int bar(Blob blob) {
    printf("blob.a: %llu\n", blob.a);
    printf("blob.b: %llu\n", blob.b);
    printf("blob.c: %llu\n", blob.c);
    printf("blob.d: %llu\n", blob.d);
    printf("blob.e: %llu\n", blob.e);
    printf("blob.f: %llu\n", blob.f);
    printf("blob.g: %llu\n", blob.g);
    printf("blob.h: %llu\n", blob.h);
    printf("blob.i: %llu\n", blob.i);
    printf("blob.j: %llu\n", blob.j);
    printf("blob.k: %llu\n", blob.k);
    return 600;
}

int main(int argc, char *argv[]) {

    int x = 0;
    if (x) {
        printf("it is true\n");
    }
    else {
        printf("it is false\n");
    }

    printf("hi there\n");
    foo(100);
    printf("more fun\n");
    
    Blob blob;
    blob.a = 301;
    blob.b = 302;
    blob.c = 303;
    blob.d = 304;
    blob.e = 305;
    blob.f = 306;
    blob.g = 307;
    blob.h = 308;
    blob.i = 309;
    blob.j = 310;
    blob.k = 311;
    bar(blob);

    return 0;
}
