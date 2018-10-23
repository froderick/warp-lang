#include <stdio.h>
#include <stdlib.h>

typedef struct s_Bingo {
  int a, b, c;
} *Bingo;

Bingo make_bingo() {
  Bingo b = malloc(sizeof(struct s_Bingo));
  b->a = 1;
  b->b = 2;
  b->c = 3;
  return b;
}

void print_bingo(Bingo b) {
  printf("bingo: %i, %i, %i\n", b->a, b->b, b->c);
}
