#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>

#define MEMSIZE 0x8000

static void push(uint16_t);
static void vm(bool);
static int input(bool);
static inline void output(char c);
static void load_program(const char *name);
static void render_mem(char *out, size_t n, uint16_t addr);
static void trace_op(uint16_t);
static void console_help(char*);

static uint16_t reg[8];
static size_t   stack_size;
static uint16_t *stack;
static uint16_t *sp;
static bool     tracing;
static uint16_t mem[MEMSIZE];

/* Push the argument onto the stack. This function
 * reallocates the stack to be twice as big in the case
 * that the stack was full. When reallocation fails the
 * program will terminate with failure. */
static inline void push(uint16_t x) {
    if (__builtin_expect(sp == stack + stack_size,false)) {
        stack_size *= 2;
        stack = realloc(stack, stack_size * sizeof(uint16_t));
        if (!stack) exit(EXIT_FAILURE);
    }
    *sp++ = x;
}

static inline uint16_t readmem(uint16_t addr) {
    return mem[addr % MEMSIZE];
}

static void output(char c) {
    if (!tracing) {
        putchar(c);
        if (c == '\n') fflush(stdout);
    }
}

static void console_setreg(char *args) {

    char c;
    uint16_t val;
    int count = sscanf(args, "%c%hx", &c, &val);
    int r = c - 'A';

    if (count == 2 && r < 8) {
        reg[r] = val;
        printf("### Register %c set to %04hx\n", c, val);
    } else {
        printf("### Usage: setreg [A-H] VALUE\n");
    }
}

static void console_getreg(char *args) {
    printf("###");
    for (int r = 0; r < 8; r++) {
        printf("  %c:%04hx", 'A'+r, reg[r]);
    }
    printf("\n");
}

static void console_setmem(char *args) {

    uint16_t r, val;
    int count = sscanf(args, "%hx%hx", &r, &val);

    if (count == 2 && r < MEMSIZE) {
        mem[r] = val;
        printf("### Memory %04hx set to %04hx\n", r, val);
    } else {
        printf("### Usage: setmem ADDR VALUE\n");
    }
}

static void console_getmem(char *args) {

    uint16_t r;
    int count = sscanf(args, "%hx", &r);

    if (count == 1 && r < MEMSIZE) {
        printf("### Memory %04hx is %04hx\n", r, mem[r]);
    } else {
        printf("### Usage: getmem ADDR\n");
    }
}

static void console_trace(char *args) {

    char * mode = strsep(&args," \n");

    if (!strcmp(mode, "on")) {
        printf("### Tracing enabled\n");
        tracing = true;
    } else if (!strcmp(mode, "off")) {
        printf("### Tracing disabled\n");
        tracing = false;
    } else {
        printf("### Usage: trace [on|off]\n");
    }
}

static void console_dumpmem(char *args) {

    char *filename = strsep(&args," \n");
    FILE *dump = fopen(filename, "w");

    if (!dump) {
        perror("### Error dumping memory");
    } else {
        fwrite(mem, sizeof(uint16_t), MEMSIZE, dump);
        fclose(dump);
        printf("### Memory dumped to %s\n", filename);
    }
}

void console_break(char *args) {

    uint16_t addr;
    char *mode = strsep(&args, " \n");
    int count = args ? sscanf(args, "%hx", &addr) : 0;

    if (!strcmp("set",mode) && count == 1 && addr < MEMSIZE) {
        uint16_t v = mem[addr];
        if ((v & 0x1f) == 0x1f) {
            printf("### Breakpoint already set\n");
        } else {
            mem[addr] = (v << 8) | 0x1f;
            printf("### Breakpoint set\n");
        }

    } else if (!strcmp("clear",mode) && count == 1 && addr < MEMSIZE) {
        uint16_t v = mem[addr];
        if ((v & 0x1f) == 0x1f) {
            mem[addr] = v >> 8;
            printf("### Breakpoint cleared\n");
        } else {
            printf("### No breakpoint at this address\n");
        }

    } else {
        printf("### Usage: break [set|clear] ADDR\n");
    }
}

struct console_cmd {
    const char *name;
    void (*run)(char *);
};

static struct console_cmd cmds[] =
    { { "setreg" , console_setreg  }
    , { "getreg" , console_getreg  }
    , { "setmem" , console_setmem  }
    , { "getmem" , console_getmem  }
    , { "trace"  , console_trace   }
    , { "dumpmem", console_dumpmem }
    , { "break"  , console_break   }
    , { "help"   , console_help    }
    , { NULL     , NULL            }
    };

static void console_help(char *args) {
    printf("### Commands:");
    for (int i = 0; cmds[i].name; i++) {
        printf(" %s", cmds[i].name);
    }
    printf("\n");
}


static void console(void) {

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen = getline(&line, &linecap, stdin);
    if (linelen == -1) exit(EXIT_FAILURE);

    char *cmd = strsep(&line, " \n");
    while (*line == ' ') line++; /* drop whitespace */

    int i;
    for (i = 0; cmds[i].name; i++) {
      if (!strcmp(cmd, cmds[i].name)) {
        cmds[i].run(line);
        break;
      }
    }

    if (!cmds[i].name) {
      printf("### Bad console command `%s`\n", cmd);
    }

    free(cmd);
}

static int input(bool echo) {
    char c = getchar();
    size_t len;
    switch (c) {
        case EOF:                      exit(EXIT_SUCCESS);
        case '!': console();           return input(echo);
        case '#': fgetln(stdin, &len); return input(echo);
        default:                       if (echo) {putchar(c);} return c;
    }
}

static void render_mem(char *out, size_t n, uint16_t addr) {
    uint16_t val = readmem(addr);
    if (val & MEMSIZE) {
        snprintf(out, n, "%c", 'A' + (val&7));
    } else {
        snprintf(out, n, "%04hx", val);
    }
    out[n-1] = '\0';
}

static void trace_op(uint16_t pc) {

    /* a jump table is unnecessary here, but I had one laying around... */
    static void *dispatch_table[] = {
        &&HALT, &&SET,  &&PUSH, &&POP, &&EQ,  &&GT,   &&JMP, &&JT,
        &&JF,   &&ADD,  &&MULT, &&MOD, &&AND, &&OR,   &&NOT, &&RMEM,
        &&WMEM, &&CALL, &&RET,  &&OUT, &&IN,  &&NOOP, &&BAD, &&BAD,
        &&BAD,  &&BAD,  &&BAD,  &&BAD, &&BAD, &&BAD,  &&BAD, &&BREAK
    };

    char a[7], b[7], c[7];
    render_mem(a,sizeof(a),pc+1);
    render_mem(b,sizeof(b),pc+2);
    render_mem(c,sizeof(c),pc+3);

    goto *dispatch_table[readmem(pc) & 0x1f];
    SET:  printf("%04hx: %s = %s\n",       pc, a, b   ); return;
    EQ:   printf("%04hx: %s = %s == %s\n", pc, a, b, c); return;
    GT:   printf("%04hx: %s = %s > %s\n",  pc, a, b, c); return;
    ADD:  printf("%04hx: %s = %s + %s\n",  pc, a, b, c); return;
    MULT: printf("%04hx: %s = %s * %s\n",  pc, a, b, c); return;
    MOD:  printf("%04hx: %s = %s %% %s\n", pc, a, b, c); return;
    AND:  printf("%04hx: %s = %s & %s\n",  pc, a, b, c); return;
    OR:   printf("%04hx: %s = %s | %s\n",  pc, a, b, c); return;
    NOT:  printf("%04hx: %s = ~%s\n",      pc, a, b   ); return;
    RMEM: printf("%04hx: %s = mem[%s]\n",  pc, a, b   ); return;
    WMEM: printf("%04hx: mem[%s] = %s\n",  pc, a, b   ); return;

    JMP:  printf("%04hx: JMP %s\n",        pc, a      ); return;
    JT:   printf("%04hx: JT [%s] %s\n",    pc, a, b   ); return;
    JF:   printf("%04hx: JF [%s] %s\n",    pc, a, b   ); return;

    OUT:  { uint16_t v = readmem(pc+1);
            if (0x20 <= v && v < 0x80)
                printf("%04hx: OUTPUT '%c'\n", pc, (char)v);
            else
                printf("%04hx: OUTPUT %s\n", pc, a);
            return;
          }
    IN:   printf("%04hx: INPUT %s\n",      pc, a      ); return;

    NOOP: printf("%04hx: NOP\n",           pc         ); return;
    PUSH: printf("%04hx: PUSH %s\n",       pc, a      ); return;
    POP:  printf("%04hx: POP %s\n",        pc, a      ); return;
    CALL: printf("%04hx: CALL %s\n",       pc, a      ); return;
    RET:  printf("%04hx: RET\n",           pc         ); return;

    HALT: printf("%04hx: HALT\n",          pc         ); return;
    BAD:  printf("%04hx: BAD\n",           pc         ); return;
    BREAK:                                               return;
}

static void load_program(const char *name) {

    FILE *pgm = fopen(name, "r");
    if (!pgm) {
        perror("Failed opening program:");
        exit(EXIT_FAILURE);
    }
    fread(mem, sizeof(uint16_t), MEMSIZE, pgm);
    fclose(pgm);
}

void usage(void) {
    fprintf(stderr, "Usage: vm [options] IMAGE\n"
                    "Available options are:\n"
                    "  -d       Set breakpoint at startup\n"
                    "  -e       Echo inputs\n"
                    "  -t       Start with trace enabled\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {

    bool dflag = false;
    bool eflag = false;
    int ch;

    while ((ch = getopt(argc, argv, "det")) != -1) {
        switch(ch) {
            case 'd': dflag   = true; break;
            case 'e': eflag   = true; break;
            case 't': tracing = true; break;
            case '?': usage();
            default: usage();
        }
    }
    argc -= optind;
    argv += optind;

    if (argc < 1) usage();

    load_program(argv[0]);

    if (dflag) { mem[0] = (mem[0] << 8) | 0x1f; }

    vm(eflag);

    return 0;
}

/* Look up value indicated by opcode argument.
 * Values beyond the address space indicate register names */
static inline uint16_t arg(int x) {
    const uint16_t v = readmem(x);
    return v & MEMSIZE ? reg[v & 0x7] : v;
}

static void vm(bool echo) {

    uint16_t pc = 0;
    stack_size = 0x8000; /* initial guess */
    stack      = malloc(stack_size * sizeof(uint16_t));
    sp         = stack;

    #define AR (reg[readmem(pc+1) & 0x7])
    #define A arg(pc+1)
    #define B arg(pc+2)
    #define C arg(pc+3)

    static void *dispatch_table[] = {
        &&HALT, &&SET,  &&PUSH, &&POP, &&EQ,  &&GT,   &&JMP, &&JT,
        &&JF,   &&ADD,  &&MULT, &&MOD, &&AND, &&OR,   &&NOT, &&RMEM,
        &&WMEM, &&CALL, &&RET,  &&OUT, &&IN,  &&NOOP, &&BAD, &&BAD,
        &&BAD,  &&BAD,  &&BAD,  &&BAD, &&BAD, &&BAD,  &&BAD, &&BREAK
    };

    #define DISPATCH() \
      do { if (__builtin_expect(tracing,false)) trace_op(pc); \
           goto *dispatch_table[readmem(pc) & 0x1f]; \
         } while(false)

    DISPATCH();

    SET:  AR = B;                 pc += 3;                DISPATCH();
    EQ:   AR = B == C;            pc += 4;                DISPATCH();
    GT:   AR = B > C;             pc += 4;                DISPATCH();
    JMP:                          pc = A;                 DISPATCH();
    JT:                           pc = A  ? B : pc+3;     DISPATCH();
    JF:                           pc = !A ? B : pc+3;     DISPATCH();
    ADD:  AR = (B + C) & 0x7fff;  pc += 4;                DISPATCH();
    MULT: AR = (B * C) & 0x7fff;  pc += 4;                DISPATCH();
    MOD:  AR = B % C;             pc += 4;                DISPATCH();
    AND:  AR = B & C;             pc += 4;                DISPATCH();
    OR:   AR = B | C;             pc += 4;                DISPATCH();
    NOT:  AR = B ^ 0x7fff;        pc += 3;                DISPATCH();
    RMEM: AR = readmem(B);        pc += 3;                DISPATCH();
    WMEM: mem[A % MEMSIZE] = B;   pc += 3;                DISPATCH();
    OUT:  output(A);              pc += 2;                DISPATCH();
    IN:   AR = input(echo);       pc += 2;                DISPATCH();
    NOOP:                         pc += 1;                DISPATCH();
    PUSH: push(A);                pc += 2;                DISPATCH();
    POP:  if (__builtin_expect(sp == stack,false)) exit(EXIT_FAILURE);
          AR = *--sp;             pc += 2;                DISPATCH();
    CALL: push(pc+2);             pc = A;                 DISPATCH();
    RET:  if (__builtin_expect(sp == stack,false)) goto HALT;
                                  pc = *--sp;             DISPATCH();
    HALT: free(stack); sp = stack = NULL; stack_size = 0; return;
    BAD:  exit(EXIT_FAILURE);
    BREAK:
        printf("### Breakpoint at %04hx\n!", pc);
        console();
        DISPATCH();
}
