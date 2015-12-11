#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

#define MEMSIZE 0x8000

static void more_stack(size_t *sz, uint16_t **stack);
static void vm(uint16_t *mem);
static inline void output(char c);
static void load_program(const char *name, void *mem, size_t n);

static void more_stack(size_t *sz, uint16_t **stack) {
  *sz *= 2;
  *stack = realloc(*stack, (*sz) * sizeof(uint16_t));
  if (!*stack) exit(EXIT_FAILURE);
}

static inline void output(char c) {
  putchar(c);
  if (c == '\n') fflush(stdout);
}

static void load_program(const char *name, void *mem, size_t n) {

  int pgm = open(name, O_RDONLY);
  if (pgm == -1) {
    perror("open");
    exit(EXIT_FAILURE);
  }

  ssize_t sz;
  while ( (sz = read(pgm, mem, n)) > 0 ) {
    mem += sz;
    n   -= sz;
  }

  if (sz == -1) {
    perror("read");
    exit(EXIT_FAILURE);
  }

  close(pgm);
}

int main(int argc, char *argv[]) {

  uint16_t mem[MEMSIZE];

  if (argc < 2) {
    fprintf(stderr, "Usage: %s challenge.bin\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  load_program(argv[1], &mem, sizeof(mem));
  vm(mem);
  return 0;
}

static void vm(uint16_t *mem) {

  size_t pc          = 0;
  size_t stack_size  = 0x8000; /* initial guess */
  uint16_t *stack    = malloc(stack_size * sizeof(uint16_t));
  uint16_t *sp       = stack;
  uint16_t reg[8]    = {0};

  #define AR (reg[mem[pc+1] & 0x7])
  #define R(x) ((mem[pc+x] & MEMSIZE) ? reg[mem[pc+x] & 0x7] : mem[pc+x])
  #define A R(1)
  #define B R(2)
  #define C R(3)

  static void *dispatch_table[] = {
    &&HALT, &&SET,  &&PUSH, &&POP, &&EQ,  &&GT,   &&JMP, &&JT,
    &&JF,   &&ADD,  &&MULT, &&MOD, &&AND, &&OR,   &&NOT, &&RMEM,
    &&WMEM, &&CALL, &&RET,  &&OUT, &&IN,  &&NOOP, &&BAD, &&BAD,
    &&BAD,  &&BAD,  &&BAD,  &&BAD, &&BAD, &&BAD,  &&BAD, &&BAD
  };
  #define DISPATCH() goto *dispatch_table[mem[pc] & 0x1f]

  DISPATCH();

  SET:  AR = B;                 pc += 3;                DISPATCH();
  EQ:   AR = B == C;            pc += 4;                DISPATCH();
  GT:   AR = B > C;             pc += 4;                DISPATCH();
  JMP:                          pc = A;                 DISPATCH();
  JT:                           pc = A  ? B : pc+3;     DISPATCH();
  JF:                           pc = !A ? B : pc+3;     DISPATCH();
  ADD:  AR = (B + C) % 0x8000;  pc += 4;                DISPATCH();
  MULT: AR = (B * C) % 0x8000;  pc += 4;                DISPATCH();
  MOD:  AR = B % C;             pc += 4;                DISPATCH();
  AND:  AR = B & C;             pc += 4;                DISPATCH();
  OR:   AR = B | C;             pc += 4;                DISPATCH();
  NOT:  AR = B ^ 0x7fff;        pc += 3;                DISPATCH();
  RMEM: AR = mem[B];            pc += 3;                DISPATCH();
  WMEM: mem[A] = B;             pc += 3;                DISPATCH();
  OUT:  output(A);              pc += 2;                DISPATCH();
  IN:   AR = getchar();         pc += 2;                DISPATCH();
  NOOP:                         pc += 1;                DISPATCH();
  PUSH: if (sp == stack + stack_size) more_stack(&stack_size, &stack);
        *sp++ = A;              pc += 2;                DISPATCH();
  POP:  if (sp == stack) exit(EXIT_FAILURE);
        AR = *--sp;             pc += 2;                DISPATCH();
  CALL: if (sp == stack + stack_size) more_stack(&stack_size, &stack);
        *sp++ = pc+2;           pc = A;                 DISPATCH();
  RET:  if (sp == stack) goto HALT;
                                pc = *--sp;             DISPATCH();
  HALT: free(stack);                                    return;
  BAD:  exit(EXIT_FAILURE);
}
