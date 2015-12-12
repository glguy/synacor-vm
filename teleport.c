#include <stdlib.h>
#include <string.h>
#include <stdio.h>

uint16_t cache[5][0x8000];

uint16_t h; /*register 8*/
static uint16_t top(uint16_t a, uint16_t b) {

  if (cache[a][b] != 0xffff) return cache[a][b];

  if (a == 0) {
     return cache[a][b] = (b + 1) & 0x7fff;
  }

  if (b == 0) {
     return cache[a][b] = top(a-1,h);
  }

  return cache[a][b] = top(a-1, top(a,b-1));
}

int main (int argc, char *argv[]) {
   for (h = 1; h < 0x8000; h++) {
        memset(cache, 0xff, sizeof(cache));
        uint16_t r = top(4,1);
        if (! (h&0xff)) { printf("."); fflush(stdout); }
        if (r == 6) {
          printf("\n%#hx\n", h);
          return 0;
        }
   }
}
