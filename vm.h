#ifndef VM_H
#define VM_H
#include <stdint.h>

/* VM opcode enum
   Naming: VM_OP_<mnemonic>
*/
typedef enum {
    VM_OP_NOP = 0,       /* no-op */
    VM_OP_ADD = 0x1, // 0 0001 // reg-reg expects next u8 to be first 4 bits first reg and last 4 bits to be last reg, last reg to first reg
    VM_OP_SUB = 0x2,
    VM_OP_MOV = 0x3,
    VM_OP_CMP = 0x4,
    VM_OP_JMP = 0x5,
    VM_OP_AND = 0x6,
    VM_OP_OR = 0x7,
    VM_OP_NAND = 0x8,
    VM_OP_XOR = 0x9,
    VM_OP_SHL = 0xa,
    VM_OP_SHR = 0xb,
    VM_OP_MUL = 0xc,
    VM_OP_DIV = 0xd,
    VM_OP_PUSH = 0xe,
    VM_OP_POP = 0xf, // 0 1111
    VM_OP_MOD = 0x10, // 1 0000
    VM_OP_INC = 0x11, // 1 0001
    VM_OP_DEC = 0x12, // 1 0010
    VM_OP_NEG = 0x13, // 1 0011
    // VM_OP_JE,
    VM_OP_JNE = 0x14,
    VM_OP_JG = 0x15,
    VM_OP_JL = 0x16,
    VM_OP_JGE = 0x17,
    VM_OP_JLE = 0x18,
    /* -- System / control --------------------------------- */
    VM_OP_HLT = 0x19,           /* halt the VM */
    /*first 5 bits are op, last 3 are type */

    /* keep this last so you can allocate space easily */
    VM_OP_COUNT,

} _VM_OP;
typedef enum {
    VM_OP_REG_REG = 0x1, // 001
    VM_OP_REG_IM = 0x2,
    VM_OP_REG_MEM = 0x3,
    VM_OP_MEM_REG = 0x4,
} _VM_OP_TYPE;
static uint8_t make_instruction(_VM_OP op, _VM_OP_TYPE type) { return op << 3 | type ; }
typedef enum {
    _nonde,
} VM_OP;

// cpu8.h  — simple 8-bit CPU structure and core helpers
// (single-file example; adapt types & sizes as you like)

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* -------------------------
   CPU layout / constants
   ------------------------- */
#define MEM_SIZE 65536  /* 64 KiB address space (16-bit addresses) */
#define NUM_REGS 8      /* 8 general-purpose 8-bit registers R0..R7 */

typedef enum {
    FLAG_Z = 1<<7, /* Zero */
    FLAG_N = 1<<6, /* Negative / Subtract (if you want) */
    FLAG_H = 1<<5, /* Half-carry (BCD helpers) */
    FLAG_C = 1<<4  /* Carry */
    /* remaining low bits free for custom flags */
} FlagBits;

/* Human-friendly names for the 8 registers (optional) */
enum { R0 = 0, R1, R2, R3, R4, R5, R6, R7 };

/* Single CPU structure */
typedef struct {
    /* 8-bit general purpose registers */
    uint8_t reg[NUM_REGS]; /* reg[0..7] */

    /* Special registers */
    uint16_t pc;   /* program counter (16-bit) */
    uint16_t sp;   /* stack pointer (16-bit) */

    /* Flags register (pack flags into one byte) */
    uint8_t flags; /* bitfield of FlagBits */

    /* Memory (owner may be CPU or passed pointer). Here we allocate. */
    uint8_t *memory; /* pointer to MEM_SIZE bytes */

    /* Cycle counting, helpful for timing/perf */
    uint64_t cycles;

    /* I/O / host callbacks (optional) */
    // void (*on_trap)(struct cpu8_t *cpu, uint8_t trap_no);

    /* convenience: is the cpu halted? */
    bool halted;
} cpu8_t;

/* -------------------------
   Helpers: flags & memory
   ------------------------- */

static inline void set_flag(cpu8_t *cpu, FlagBits f) { cpu->flags |= (uint8_t)f; }
static inline void clear_flag(cpu8_t *cpu, FlagBits f) { cpu->flags &= ~(uint8_t)f; }
static inline bool test_flag(const cpu8_t *cpu, FlagBits f) { return (cpu->flags & (uint8_t)f) != 0; }
static inline void write_flag(cpu8_t *cpu, FlagBits f, bool val) { if(val) set_flag(cpu,f); else clear_flag(cpu,f); }

static inline uint8_t mem_read8(cpu8_t *cpu, uint16_t addr) {
    return cpu->memory[addr];
}
static inline void mem_write8(cpu8_t *cpu, uint16_t addr, uint8_t val) {
    cpu->memory[addr] = val;
}
static inline uint16_t mem_read16(cpu8_t *cpu, uint16_t addr) {
    /* little-endian read */
    uint8_t lo = mem_read8(cpu, addr);
    uint8_t hi = mem_read8(cpu, addr + 1);
    return (uint16_t)lo | ((uint16_t)hi << 8);
}
static inline void mem_write16(cpu8_t *cpu, uint16_t addr, uint16_t v) {
    mem_write8(cpu, addr, (uint8_t)(v & 0xFF));
    mem_write8(cpu, addr+1, (uint8_t)(v >> 8));
}

/* -------------------------
   Create / reset / destroy
   ------------------------- */
static void cpu8_reset(cpu8_t *cpu) {
    memset(cpu->reg, 0, sizeof(cpu->reg));
    cpu->pc = 0x0000;
    cpu->sp = 0xFFFE; /* common default: stack near top of 64K */
    cpu->flags = 0;
    cpu->cycles = 0;
    cpu->halted = false;
    /* memory intentionally preserved by default — clear if desired: */
    /* memset(cpu->memory, 0, MEM_SIZE); */
}

static cpu8_t *cpu8_create(void) {
    cpu8_t *c = (cpu8_t*)malloc(sizeof(cpu8_t));
    if (!c) return NULL;
    c->memory = (uint8_t*)malloc(MEM_SIZE);
    if (!c->memory) { free(c); return NULL; }
    cpu8_reset(c);
    return c;
}


static void cpu8_destroy(cpu8_t *cpu) {
    if (!cpu) return;
    free(cpu->memory);
    free(cpu);
}

/* -------------------------
   Small example instruction set
   and helpers to fetch / decode
   ------------------------- */

/* Example opcodes (very small set for demonstration) */
enum {
    OP_NOP = 0x00,
    OP_HLT = 0x01,

    OP_MOV_R_R = 0x10,  /* MOV rA, rB (byte: opcode, operand) */
    OP_MOV_R_IMM = 0x11,/* MOV rA, imm8 (opcode, reg, imm8) */

    OP_ADD_R_R = 0x20,  /* ADD rA, rB -> rA (reg + reg) */
    OP_ADD_R_IMM = 0x21,/* ADD rA, imm8 */

    OP_JMP = 0x30,      /* JMP addr16 (opcode, addr_lo, addr_hi) */
    OP_JZ = 0x31,       /* JZ addr16 (if zero flag set) */

    OP_PUSH_R = 0x40,   /* PUSH rA (opcode, reg) */
    OP_POP_R  = 0x41,   /* POP rA  (opcode, reg) */

    OP_CALL = 0x50,     /* CALL addr16 */
    OP_RET  = 0x51,     /* RET */

    /* extend with more opcodes as needed */
};

/* fetch helpers */
static inline uint8_t fetch8(cpu8_t *cpu) {
    uint8_t v = mem_read8(cpu, cpu->pc);
    cpu->pc += 1;
    return v;
}
static inline uint16_t fetch16(cpu8_t *cpu) {
    uint16_t v = mem_read16(cpu, cpu->pc);
    cpu->pc += 2;
    return v;
}

/* push/pull helpers (stack grows downward) */
static void push16(cpu8_t *cpu, uint16_t v) {
    cpu->sp -= 2;
    mem_write16(cpu, cpu->sp, v);
}
static uint16_t pop16(cpu8_t *cpu) {
    uint16_t v = mem_read16(cpu, cpu->sp);
    cpu->sp += 2;
    return v;
}

/* Helper: set Z flag for 8-bit result */
static inline void update_zero_flag_8(cpu8_t *cpu, uint8_t res) {
    write_flag(cpu, FLAG_Z, res == 0);
}

/* Example instruction implementations */
static void exec_mov_r_r(cpu8_t *cpu) {
    uint8_t operand = fetch8(cpu);
    uint8_t ra = (operand >> 4) & 0x0F; /* high nibble = dest reg */
    uint8_t rb = operand & 0x0F;        /* low nibble = src reg */
    if (ra < NUM_REGS && rb < NUM_REGS) {
        cpu->reg[ra] = cpu->reg[rb];
    }
    cpu->cycles += 1;
}

static void exec_mov_r_imm(cpu8_t *cpu) {
    uint8_t regno = fetch8(cpu) & 0x0F;
    uint8_t imm = fetch8(cpu);
    if (regno < NUM_REGS) cpu->reg[regno] = imm;
    cpu->cycles += 2;
}

static void exec_add_r_r(cpu8_t *cpu) {
    uint8_t operand = fetch8(cpu);
    uint8_t ra = (operand >> 4) & 0x0F;
    uint8_t rb = operand & 0x0F;
    if (ra < NUM_REGS && rb < NUM_REGS) {
        uint16_t sum = (uint16_t)cpu->reg[ra] + (uint16_t)cpu->reg[rb];
        cpu->reg[ra] = (uint8_t)(sum & 0xFF);
        write_flag(cpu, FLAG_C, (sum & 0x100) != 0);
        update_zero_flag_8(cpu, cpu->reg[ra]);
    }
    cpu->cycles += 1;
}

static void exec_add_r_imm(cpu8_t *cpu) {
    uint8_t regno = fetch8(cpu) & 0x0F;
    uint8_t imm = fetch8(cpu);
    if (regno < NUM_REGS) {
        uint16_t sum = (uint16_t)cpu->reg[regno] + imm;
        cpu->reg[regno] = (uint8_t)(sum & 0xFF);
        write_flag(cpu, FLAG_C, (sum & 0x100) != 0);
        update_zero_flag_8(cpu, cpu->reg[regno]);
    }
    cpu->cycles += 2;
}

/* simple CALL/RET */
static void exec_call(cpu8_t *cpu) {
    uint16_t addr = fetch16(cpu);
    push16(cpu, cpu->pc); /* push return address */
    cpu->pc = addr;
    cpu->cycles += 4;
}
static void exec_ret(cpu8_t *cpu) {
    cpu->pc = pop16(cpu);
    cpu->cycles += 4;
}

/* JMP / JZ */
static void exec_jmp(cpu8_t *cpu) {
    uint16_t addr = fetch16(cpu);
    cpu->pc = addr;
    cpu->cycles += 3;
}
static void exec_jz(cpu8_t *cpu) {
    uint16_t addr = fetch16(cpu);
    if (test_flag(cpu, FLAG_Z)) cpu->pc = addr;
    cpu->cycles += 3;
}

/* PUSH / POP (8-bit registers) */
static void exec_push_r(cpu8_t *cpu) {
    uint8_t regno = fetch8(cpu) & 0x0F;
    if (regno < NUM_REGS) {
        cpu->sp -= 1;
        mem_write8(cpu, cpu->sp, cpu->reg[regno]);
    }
    cpu->cycles += 2;
}
static void exec_pop_r(cpu8_t *cpu) {
    uint8_t regno = fetch8(cpu) & 0x0F;
    if (regno < NUM_REGS) {
        cpu->reg[regno] = mem_read8(cpu, cpu->sp);
        cpu->sp += 1;
    }
    cpu->cycles += 2;
}

/* -------------------------
   Main fetch-decode-execute
   ------------------------- */
static void cpu8_step(cpu8_t *cpu) {
    if (cpu->halted) return;
    uint8_t op = fetch8(cpu);
    switch (op) {
        case OP_NOP: break;
        case OP_HLT: cpu->halted = true; break;

        case OP_MOV_R_R: exec_mov_r_r(cpu); break;
        case OP_MOV_R_IMM: exec_mov_r_imm(cpu); break;

        case OP_ADD_R_R: exec_add_r_r(cpu); break;
        case OP_ADD_R_IMM: exec_add_r_imm(cpu); break;

        case OP_JMP: exec_jmp(cpu); break;
        case OP_JZ: exec_jz(cpu); break;

        case OP_PUSH_R: exec_push_r(cpu); break;
        case OP_POP_R: exec_pop_r(cpu); break;

        case OP_CALL: exec_call(cpu); break;
        case OP_RET: exec_ret(cpu); break;

        default:
            /* unknown opcode: trap or halt */
            fprintf(stderr, "Unhandled opcode 0x%02X at PC=0x%04X\n", op, cpu->pc-1);
            cpu->halted = true;
            break;
    }
}

/* Run N steps (or until halted) */
static void cpu8_run(cpu8_t *cpu, uint64_t steps) {
    for (uint64_t i = 0; i < steps && !cpu->halted; ++i) cpu8_step(cpu);
}

/* -------------------------
   Small assembler helpers
   (emit bytes into memory)
   ------------------------- */
static void emit8(cpu8_t *cpu, uint16_t *addr, uint8_t v) { mem_write8(cpu, *addr, v); (*addr)++; }
static void emit16(cpu8_t *cpu, uint16_t *addr, uint16_t v) { mem_write16(cpu, *addr, v); (*addr)+=2; }

/* Example: load a tiny program into memory */
static void load_example_program(cpu8_t *cpu) {
    uint16_t addr = 0x0000;
    /* MOV r0, 5 */
    emit8(cpu, &addr, OP_MOV_R_IMM);
    emit8(cpu, &addr, (uint8_t)0 /* r0 */);
    emit8(cpu, &addr, 5);
    /* MOV r1, 10 */
    emit8(cpu, &addr, OP_MOV_R_IMM);
    emit8(cpu, &addr, (uint8_t)1 /* r1 */);
    emit8(cpu, &addr, 10);
    /* ADD r0, r1  (r0 = r0 + r1) */
    emit8(cpu, &addr, OP_ADD_R_R);
    emit8(cpu, &addr, (uint8_t)((0<<4) | 1)); /* dest r0, src r1 */
    /* HLT */
    emit8(cpu, &addr, OP_HLT);

    /* set start PC */
    cpu->pc = 0x0000;
}

/* -------------------------
   Example usage (main)
   ------------------------- */
#ifdef CPU8_STANDALONE_TEST
int main(void) {
    cpu8_t *c = cpu8_create();
    if (!c) return 1;
    load_example_program(c);
    cpu8_run(c, 1000);
    printf("R0=%u R1=%u flags=0x%02X cycles=%llu\n",
           (unsigned)c->reg[0], (unsigned)c->reg[1], (unsigned)c->flags, (unsigned long long)c->cycles);
    cpu8_destroy(c);
    return 0;
}
#endif



#endif // VM_H
