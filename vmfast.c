#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>

#define MEMSIZE 0x8000

static void vm(uint16_t*);
static void load_program(uint16_t *mem, const char *name);

static void console_setreg(uint16_t *mem, char *args) {

    char c;
    uint16_t val;
    int count = sscanf(args, "%c%hx", &c, &val);
    int r = c - 'A';

    if (count == 2 && r < 8) {
        mem[MEMSIZE+r] = val;
        printf("### Register %c set to %04hx\n", c, val);
    } else {
        printf("### Usage: setreg [A-H] VALUE\n");
    }
}

static void console_getreg(uint16_t *mem, char *args) {
    printf("###");
    for (int r = 0; r < 8; r++) {
        printf("  %c:%04hx", 'A'+r, mem[MEMSIZE+r]);
    }
    printf("\n");
}

static void console_setmem(uint16_t *mem, char *args) {

    uint16_t r, val;
    int count = sscanf(args, "%hx%hx", &r, &val);

    if (count == 2 && r < MEMSIZE) {
        mem[r] = val;
        printf("### Memory %04hx set to %04hx\n", r, val);
    } else {
        printf("### Usage: setmem ADDR VALUE\n");
    }
}

static void console_getmem(uint16_t *mem, char *args) {

    uint16_t r;
    int count = sscanf(args, "%hx", &r);

    if (count == 1 && r < MEMSIZE) {
        printf("### Memory %04hx is %04hx\n", r, mem[r]);
    } else {
        printf("### Usage: getmem ADDR\n");
    }
}

static void console_dumpmem(uint16_t *mem, char *args) {

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

void console_break(uint16_t *mem, char *args) {

    uint16_t addr;
    char *mode = strsep(&args, " \n");
    int count = args ? sscanf(args, "%hx", &addr) : 0;

    if (!strcmp("set",mode) && count == 1 && addr < MEMSIZE) {
        uint16_t v = mem[addr];
        if ((v & 0x1f) == 0x1f) {
            printf("### Breakpoint already set\n");
        } else {
            mem[addr] = (v << 5) | 0x1f;
            printf("### Breakpoint set\n");
        }

    } else if (!strcmp("clear",mode) && count == 1 && addr < MEMSIZE) {
        uint16_t v = mem[addr];
        if ((v & 0x1f) == 0x1f) {
            mem[addr] = v >> 5;
            printf("### Breakpoint cleared\n");
        } else {
            printf("### No breakpoint at this address\n");
        }

    } else {
        printf("### Usage: break [set|clear] ADDR\n");
    }
}

static void console_help(uint16_t *mem, char*);

static struct {
    const char *name;
    void (*run)(uint16_t *mem, char *);
} cmds[] =
    { { "setreg" , console_setreg  }
    , { "getreg" , console_getreg  }
    , { "setmem" , console_setmem  }
    , { "getmem" , console_getmem  }
    , { "dumpmem", console_dumpmem }
    , { "break"  , console_break   }
    , { "help"   , console_help    }
    , { NULL     , NULL            }
    };

static void console_help(uint16_t *mem, char *args) {
    printf("### Commands:");
    for (int i = 0; cmds[i].name; i++) {
        printf(" %s", cmds[i].name);
    }
    printf("\n");
}

static void console(uint16_t *mem) {

    char *line = NULL;
    size_t linecap = 0;
    ssize_t linelen = getline(&line, &linecap, stdin);
    if (linelen == -1) exit(EXIT_FAILURE);

    char *cmd = strsep(&line, " \n");
    while (*line == ' ') line++; /* drop whitespace */

    int i;
    for (i = 0; cmds[i].name; i++) {
      if (!strcmp(cmd, cmds[i].name)) {
        cmds[i].run(mem, line);
        break;
      }
    }

    if (!cmds[i].name) {
      printf("### Bad console command `%s`\n", cmd);
    }

    free(cmd);
}

static int input(uint16_t *mem) {
    char c = getchar();
    static size_t len;
    switch (c) {
        case EOF:                      exit(EXIT_SUCCESS);
        case '!': console(mem);        return input(mem);
        case '#': fgetln(stdin, &len); return input(mem);
        default:                       return c;
    }
}

static void load_program(uint16_t *mem, const char *name) {

    FILE *pgm = fopen(name, "r");
    if (!pgm) {
        perror("Failed opening program:");
        exit(EXIT_FAILURE);
    }
    fread(mem, sizeof(uint16_t), MEMSIZE, pgm);
    fclose(pgm);
}

static void usage(void) {
    fprintf(stderr, "Usage: vm [options] IMAGE\n"
                    "Available options are:\n"
                    "  -d       Set breakpoint at startup\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {

    bool dflag = false;
    int ch;

    while ((ch = getopt(argc, argv, "d")) != -1) {
        switch(ch) {
            case 'd': dflag = true; break;
            case '?': usage();
            default: usage();
        }
    }
    argc -= optind;
    argv += optind;

    if (argc < 1) usage();

    uint16_t mem[0x10003] = {0};
    load_program(mem, argv[0]);
    if (dflag) {
        mem[0] = (mem[0] << 5) | 0x1f;
    }

    vm(mem);

    return 0;
}

struct stack {
    uint16_t *allocation;
    size_t sp;
    size_t size;
};

static inline void stack_initialize(struct stack *stack) {
    stack->size = 0x8000; /* initial guess */
    stack->allocation = malloc(stack->size * sizeof(uint16_t));
    if (stack->allocation == NULL) abort();
    stack->sp = 0;
}

static inline void stack_free(struct stack *stack) {
    free(stack->allocation);
}

static inline bool stack_full(struct stack *stack) {
    return stack->sp >= stack->size;
}

static inline bool stack_empty(struct stack *stack) {
    return stack->sp == 0;
}

static inline uint16_t stack_pop(struct stack *stack) {
    if (__builtin_expect(stack_empty(stack), false)) abort();
    return stack->allocation[--stack->sp];
}

static inline void stack_push(struct stack *stack, uint16_t x) {
    if (__builtin_expect(stack_full(stack), false)) {
        stack->size *= 2;
        stack->allocation = realloc(stack->allocation, stack->size * sizeof(uint16_t));
        if (stack->allocation == NULL) abort();
    }
    stack->allocation[stack->sp++] = x;
}

static inline uint16_t arg (uint16_t *mem, uint16_t x) {
    const uint16_t v = mem[x];
    return v & MEMSIZE ? mem[v] : v;
}

static void vm(uint16_t * mem) {

    uint16_t pc;
    struct stack stack;
    stack_initialize(&stack);

    #define AR mem[mem[pc+1]]
    #define A arg(mem, pc+1)
    #define B arg(mem, pc+2)
    #define C arg(mem, pc+3)

    #define BAD8 &&BAD,&&BAD,&&BAD,&&BAD,&&BAD,&&BAD,&&BAD,&&BAD
    #define BAD32 BAD8,BAD8,BAD8,BAD8
    static const void *dispatch_table[0x100] = {
        &&HALT, &&SET,  &&PUSH, &&POP, &&EQ,  &&GT,   &&JMP, &&JT,
        &&JF,   &&ADD,  &&MULT, &&MOD, &&AND, &&OR,   &&NOT, &&RMEM,
        &&WMEM, &&CALL, &&RET,  &&OUT, &&IN,  &&NOOP, &&BAD, &&BREAK,
        BAD8, BAD32, BAD32, BAD32, BAD32, BAD32, BAD32, BAD32
    };

    #define DISPATCH(pc1) goto *dispatch_table[mem[pc = (pc1)] & 0xff]

    DISPATCH(0);

    SET:  AR = B;                   DISPATCH(pc+3);
    EQ:   AR = B == C;              DISPATCH(pc+4);
    GT:   AR = B > C;               DISPATCH(pc+4);
    JMP:                            DISPATCH(A);
    JT:                             DISPATCH( A ? B : pc+3);
    JF:                             DISPATCH(!A ? B : pc+3);
    ADD:  AR = (B + C) & 0x7fff;    DISPATCH(pc+4);
    MULT: AR = (B * C) & 0x7fff;    DISPATCH(pc+4);
    MOD:  AR = B % C;               DISPATCH(pc+4);
    AND:  AR = B & C;               DISPATCH(pc+4);
    OR:   AR = B | C;               DISPATCH(pc+4);
    NOT:  AR = B ^ 0x7fff;          DISPATCH(pc+3);
    RMEM: AR = mem[B];              DISPATCH(pc+3);
    WMEM: mem[A] = B;               DISPATCH(pc+3);
    OUT:  putchar(A);               DISPATCH(pc+2);
    IN:   AR = input(mem);          DISPATCH(pc+2);
    NOOP:                           DISPATCH(pc+1);
    PUSH: stack_push(&stack, A);    DISPATCH(pc+2);
    POP:  AR = stack_pop(&stack);   DISPATCH(pc+2);
    CALL: stack_push(&stack, pc+2); DISPATCH(A);
    HALT: stack_free(&stack); return;
    BAD:  abort();
    RET:  if (__builtin_expect(stack_empty(&stack), false)) {
              goto HALT;
          } else {
              DISPATCH(stack_pop(&stack));
          }
    BREAK:
        printf("### Breakpoint at %04hx\n!", pc);
        console(mem);
        DISPATCH(pc);
}
