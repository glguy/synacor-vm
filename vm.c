#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <fcntl.h>
#include <unistd.h>

#define MEMSIZE 0x8000

static void more_stack(size_t *sz, uint16_t **stack);
static void vm(void);
static inline void output(char c);
static void load_program(const char *name, void *mem, size_t n);


size_t pc;
uint16_t mem[MEMSIZE];
uint16_t reg[8];
size_t stack_size;
uint16_t *stack;
uint16_t *sp;
bool tracing;


static void more_stack(size_t *sz, uint16_t **stack) {
  *sz *= 2;
  *stack = realloc(*stack, (*sz) * sizeof(uint16_t));
  if (!*stack) exit(EXIT_FAILURE);
}

static inline void output(char c) {
  putchar(c);
  if (c == '\n') fflush(stdout);
}

static void console(void) {
  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen = getline(&line, &linecap, stdin);

  char *cmd = strsep(&line, " \n");

  if (!strcmp(cmd,"setreg")) {
    size_t r;
    uint16_t val;
    int count = sscanf(line, "%zx%hx", &r, &val);
    if (count == 2 && r < 8) {
      reg[r] = val;
      printf("### Register %zx set to %#04hx\n", r, val);
    } else {
      printf("### Error: setreg <reg> <val>\n");
    }

  } else if (!strcmp(cmd,"getreg")) {
    size_t r;
    int count = sscanf(line, "%zx", &r);
    if (count == 1 && r < 8) {
      printf("### Register %zx is %#04hx\n", r, reg[r]);
    } else {
      printf("### Error: getreg <reg>\n");
    }

  } else if (!strcmp(cmd,"setmem")) {
    size_t r;
    uint16_t val;
    int count = sscanf(line, "%zx%hx", &r, &val);
    if (count == 2 && r < MEMSIZE) {
      mem[r] = val;
      printf("### Memory %04zx set to %#04hx\n", r, val);
    } else {
      printf("### Error: setmem <addr> <val>\n");
    }

  } else if (!strcmp(cmd,"getmem")) {
    size_t r;
    int count = sscanf(line, "%zx", &r);
    if (count == 1 && r < MEMSIZE) {
      printf("### Memory %04zx is %#04hx\n", r, mem[r]);
    } else {
      printf("### Error: getmem <addr>\n");
    }

  } else if (!strcmp(cmd,"traceon")) {
    tracing = true;
    printf("### Tracing enabled\n");

  } else if (!strcmp(cmd,"traceoff")) {
    tracing = false;
    printf("### Tracing disabled\n");

  } else if (!strcmp(cmd,"dumpmem")) {
    char * file = line;
    file = strsep(&file," \n");
    int fd = open(file, O_CREAT | O_TRUNC | O_WRONLY, 0666);
    if (fd == -1) {
      perror("### open");
    } else {
      write(fd, mem, sizeof(mem));
      close(fd);
      printf("### Memory dumped to %s\n", line);
    }

  } else {
    printf("### Bad console command `%s`\n", cmd);
  }


  free(cmd);
}

static int input(void) {
  char c = getchar();
  if (c == EOF) exit(EXIT_SUCCESS);

  if (c == '!') {
    console();
    return input();
  }

  return c;
}

static void trace(size_t pc) {

  /* a jump table is unnecessary here, but I had one laying around... */
  static void *dispatch_table[] = {
    &&HALT, &&SET,  &&PUSH, &&POP, &&EQ,  &&GT,   &&JMP, &&JT,
    &&JF,   &&ADD,  &&MULT, &&MOD, &&AND, &&OR,   &&NOT, &&RMEM,
    &&WMEM, &&CALL, &&RET,  &&OUT, &&IN,  &&NOOP, &&BAD, &&BAD,
    &&BAD,  &&BAD,  &&BAD,  &&BAD, &&BAD, &&BAD,  &&BAD, &&BAD
  };
  char a[6]="", b[6]="", c[6]="";

  if (mem[pc+1] & MEMSIZE)
    sprintf(a, "<%c>", 'A'+(mem[pc+1]&7));
  else
    sprintf(a, "%hx", mem[pc+1]);

  if (mem[pc+2] & MEMSIZE)
    sprintf(b, "<%c>", 'A'+(mem[pc+2]&7));
  else
    sprintf(b, "%hx", mem[pc+2]);

  if (mem[pc+3] & MEMSIZE)
    sprintf(c, "<%c>", 'A'+(mem[pc+3]&7));
  else
    sprintf(c, "%hx", mem[pc+3]);

  goto *dispatch_table[mem[pc] & 0x1f];
  SET:  printf("%04zx: %s = %s\n", pc, a, b); return;
  EQ:   printf("%04zx: %s = %s == %s\n", pc, a, b, c); return;
  GT:   printf("%04zx: %s = %s > %s\n", pc, a, b, c); return;
  JMP:  printf("%04zx: JMP %s\n", pc, a); return;
  JT:   printf("%04zx: JT [%s] %s\n", pc, a, b); return;
  JF:   printf("%04zx: JF [%s] %s\n", pc, a, b); return;
  ADD:  printf("%04zx: %s = %s + %s\n", pc, a, b, c); return;
  MULT: printf("%04zx: %s = %s * %s\n", pc, a, b, c); return;
  MOD:  printf("%04zx: %s = %s %% %s\n", pc, a, b, c); return;
  AND:  printf("%04zx: %s = %s & %s\n", pc, a, b, c); return;
  OR:   printf("%04zx: %s = %s | %s\n", pc, a, b, c); return;
  NOT:  printf("%04zx: %s = ~%s\n", pc, a, b); return;
  RMEM: printf("%04zx: %s = mem[%s]\n", pc, a, b); return;
  WMEM: printf("%04zx: mem[%s] = %s\n", pc, a, b); return;
  OUT:  printf("%04zx: OUTPUT %s (%c)\n", pc, a, (char)mem[pc+1]); return;
  IN:   printf("%04zx: INPUT %s\n", pc, a); return;
  NOOP: printf("%04zx: NOP\n", pc); return;
  PUSH: printf("%04zx: PUSH %s\n", pc, a); return;
  POP:  printf("%04zx: POP %s\n", pc, a); return;
  CALL: printf("%04zx: CALL %s\n", pc, a); return;
  RET:  printf("%04zx: RET\n", pc); return;
  HALT: printf("%04zx: HALT\n", pc); return;
  BAD:  printf("%04zx: BAD\n", pc); return;

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

  if (argc < 2) {
    fprintf(stderr, "Usage: %s challenge.bin\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  load_program(argv[1], &mem, sizeof(mem));
  vm();
  return 0;
}

static void vm(void) {

  pc         = 0;
  stack_size = 0x8000; /* initial guess */
  stack      = malloc(stack_size * sizeof(uint16_t));
  sp         = stack;

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
  #define DISPATCH() \
    do { if (tracing) trace(pc); goto *dispatch_table[mem[pc] & 0x1f]; } while(false)

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
  IN:   AR = input();           pc += 2;                DISPATCH();
  NOOP:                         pc += 1;                DISPATCH();
  PUSH: if (sp == stack + stack_size) more_stack(&stack_size, &stack);
        *sp++ = A;              pc += 2;                DISPATCH();
  POP:  if (sp == stack) exit(EXIT_FAILURE);
        AR = *--sp;             pc += 2;                DISPATCH();
  CALL: if (sp == stack + stack_size) more_stack(&stack_size, &stack);
        *sp++ = pc+2;           pc = A;                 DISPATCH();
  RET:  if (sp == stack) goto HALT;
                                pc = *--sp;             DISPATCH();
  HALT: free(stack); sp = stack = NULL; stack_size = 0; return;
  BAD:  exit(EXIT_FAILURE);
}
