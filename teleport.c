#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static uint16_t cache[5][0x8000];
static uint16_t h; /*register 8*/

static inline void reset_cache(void) {
  memset(cache, 0xff, sizeof(cache));
}

static inline void report_progress(void) {
  if (! (h&0xff)) {
    printf("%#06hx\r", h);
    fflush(stdout);
  }
}

/* Modified Ackermann function */
static uint16_t top(uint16_t a, uint16_t b) {

  if (cache[a][b] != 0xffff) return cache[a][b];
  if (a == 0)                return b+1 & 0x7fff;
  if (b == 0)                return top(a-1,h);

  return top(a-1, cache[a][b-1] = top(a,b-1));
}

int main (int argc, char *argv[]) {

   for (h = 1; h < 0x8000; h++) {

        reset_cache();
        report_progress();

        if (top(4,1) == 6) {
          printf("%#06hx !\n", h);
          return 0;
        }
   }

   printf("Search failed\n");
   return 1;
}

/*
 * 156b: <A> = 4
 * 156e: <B> = 1
 * 1571: CALL 178b
 * 1573: <B> = <A> == 6
 * 1577: JF [<B>] 15cb
 *
 * 178b: JT [<A>] 1793
 * 178e: <A> = <B> + 1
 * 1792: RET
 *
 * 1793: JT [<B>] 17a0
 * 1796: <A> = <A> + 7fff
 * 179a: <B> = <H>
 * 179d: CALL 178b
 * 179f: RET
 *
 * 17a0: PUSH <A>
 * 17a2: <B> = <B> + 7fff
 * 17a6: CALL 178b
 * 17a8: <B> = <A>
 * 17ab: POP <A>
 * 17ad: <A> = <A> + 7fff
 * 17b1: CALL 178b
 * 17b3: RET
 */
