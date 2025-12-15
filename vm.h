#ifndef VM_H
#define VM_H
#include <assert.h>
#include <stdint.h>
#ifdef GALA_INCLUDE
#include<gala.h>
#else
#ifndef GALA_H
#define Info(...)
#define FAILED(...) assert(0);
#endif
#endif

#define ARCH_BIT 8

/* VM opcode enum
   Naming: VM_OP_<mnemonic>
*/
typedef enum {
    /* -- System / control --------------------------------- */
    VM_OP_HLT = 0x1,           /* halt the VM */
    /* other */
    VM_OP_NOP = 0,       /* no-op */
    VM_OP_ADD = 0x2, // 0 0001 // reg-reg expects next u8 to be first 4 bits first reg and last 4 bits to be last reg, last reg to first reg
    VM_OP_SUB = 0x3,
    VM_OP_MOV = 0x4,
    VM_OP_CMP = 0x5,
    VM_OP_JMP = 0x6,
    VM_OP_AND = 0x7,
    VM_OP_OR = 0x8,
    VM_OP_NAND = 0x9,
    VM_OP_XOR = 0xa,
    VM_OP_SHL = 0xb,
    VM_OP_SHR = 0xc,
    VM_OP_MUL = 0xd,
    VM_OP_DIV = 0xe,
    VM_OP_PUSH = 0xf,
    VM_OP_POP = 0x10, // 0 1111
    VM_OP_MOD = 0x11, // 1 0000
    VM_OP_INC = 0x12, // 1 0001
    VM_OP_DEC = 0x13, // 1 0010
    VM_OP_NEG = 0x14, // 1 0011
    // VM_OP_JE,
    VM_OP_JNE = 0x15,
    VM_OP_JG = 0x16,
    VM_OP_JL = 0x17,
    VM_OP_JGE = 0x18,
    VM_OP_JLE = 0x19,
    VM_OP_JZ = 0x1a,
    VM_OP_JE = 0x1a, // same as JZ
    VM_OP_RET = 0x1b,           /* halt the VM */
    /*first 5 bits are op, last 3 are type */

    /* keep this last so you can allocate space easily */
    VM_OP_COUNT,
} _VM_OP;
typedef enum {
    VM_OP_REG_REG = 0x1, // 001
    VM_OP_REG_IM = 0x2,  // 010 im is 16 bits
    VM_OP_REG_MEM = 0x3, // 011 mem is 16 bits
    VM_OP_MEM_REG = 0x4, // 100 mem is 16 bits
    VM_OP_MEM_MEM = 0x5, // 101 mem is 16 bits each
    VM_OP_PC_MEM = 0x6, // 110 mem is 16 bits each
    VM_OP_MEM_PC = 0x7, // 111 mem is 16 bits each
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
    // FLAG_H = 1<<5, /* Half-carry (BCD helpers) */
    FLAG_C = 1<<4,  /* Carry */
    FLAG_V = 1<<3,  /* overflow for signed */
    /* remaining low bits free for custom flags */
} FlagBits;

/* Human-friendly names for the 8 registers. 15 regs max (4bit representation)*/
enum { R0 = 0, R1, R2, R3, R4, R5, R6, R7, RPC, RSP };

/* Single CPU structure */
typedef struct {
    /* 8-bit general purpose registers */
    uint8_t reg[NUM_REGS + 2]; /* reg[0..7] */

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

// check if overflows
static inline bool is_v_overflow(uint8_t a, uint8_t b, uint8_t result){
    return ((a ^ result) & 0x80) && !((a ^ b) & 0x80);
}

static inline void set_flag(cpu8_t *cpu, FlagBits f) { cpu->flags |= (uint8_t)f; }
static inline void write_flag_if(cpu8_t *cpu, FlagBits f, bool condition) { if(condition) cpu->flags |= (uint8_t)f; }
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
    cpu->sp = 0xFFFE; /* common default: stack near top of 64K */ // top
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


static inline void do_op(cpu8_t* cpu, uint8_t op, uint8_t* dest, uint8_t src) {
    uint16_t tmp16;
    uint8_t result;

    switch(op) {
        case VM_OP_ADD:
            tmp16 = (uint16_t)*dest + (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, tmp16 > 0xFF); // unsigned carry
            write_flag(cpu, FLAG_V, ((*dest ^ src) & 0x80) == 0 && ((*dest ^ result) & 0x80) != 0);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            break;

        case VM_OP_SUB:
            tmp16 = (uint16_t)*dest - (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, *dest >= src); // no borrow
            write_flag(cpu, FLAG_V, ((*dest ^ src) & 0x80) != 0 && ((*dest ^ result) & 0x80) != 0);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            break;

        case VM_OP_MUL:
            tmp16 = (uint16_t)*dest * (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, tmp16 > 0xFF);
            write_flag(cpu, FLAG_V, 0); // often V undefined for MUL
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            break;

        case VM_OP_DIV:
            if(src == 0) FAILED("Division by zero");
            result = *dest / src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_MOD:
            if(src == 0) FAILED("Modulo by zero");
            result = *dest % src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_AND:
            result = *dest & src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_OR:
            result = *dest | src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_XOR:
            result = *dest ^ src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_NAND:
            result = ~(*dest & src);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_SHL:
            write_flag(cpu, FLAG_C, (*dest & 0x80) != 0); // bit shifted out
            result = (*dest << 1) & 0xFF;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_SHR:
            write_flag(cpu, FLAG_C, (*dest & 0x01) != 0); // bit shifted out
            result = (*dest >> 1);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, 0); // logical right shift clears N
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_INC:
            tmp16 = (uint16_t)*dest + 1;
            result = tmp16 & 0xFF;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, tmp16 > 0xFF);
            write_flag(cpu, FLAG_V, ((*dest ^ 1) & 0x80) == 0 && ((*dest ^ result) & 0x80) != 0);
            break;

        case VM_OP_DEC:
            tmp16 = (uint16_t)*dest - 1;
            result = tmp16 & 0xFF;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, *dest >= 1); // borrow convention
            write_flag(cpu, FLAG_V, ((*dest ^ 1) & 0x80) != 0 && ((*dest ^ result) & 0x80) != 0);
            break;

        case VM_OP_NEG:
            result = (~(*dest) + 1) & 0xFF; // two’s complement negation
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            write_flag(cpu, FLAG_C, *dest != 0); // C = 1 if result != 0 (6502 style)
            write_flag(cpu, FLAG_V, *dest == 0x80); // signed overflow if -128
            break;

        case VM_OP_MOV:
            *dest = src;
            write_flag(cpu, FLAG_Z, *dest == 0);
            write_flag(cpu, FLAG_N, *dest & 0x80);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_CMP:
            tmp16 = (uint16_t)*dest - (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, *dest >= src);
            write_flag(cpu, FLAG_V, ((*dest ^ src) & 0x80) != 0 && ((*dest ^ result) & 0x80) != 0);
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x80);
            break;

        default: FAILED("Unsupported op %02x", op);
    }
}
static inline void do_op_16(cpu8_t* cpu, uint8_t op, uint16_t* dest, uint16_t src) {
    uint16_t tmp16;
    uint8_t result;

    switch(op) {
        case VM_OP_ADD:
            tmp16 = (uint16_t)*dest + (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, tmp16 > 0xFF); // unsigned carry
            write_flag(cpu, FLAG_V, ((*dest ^ src) & 0x8000) == 0 && ((*dest ^ result) & 0x8000) != 0);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            break;

        case VM_OP_SUB:
            tmp16 = (uint16_t)*dest - (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, *dest >= src); // no borrow
            write_flag(cpu, FLAG_V, ((*dest ^ src) & 0x8000) != 0 && ((*dest ^ result) & 0x8000) != 0);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            break;

        case VM_OP_MUL:
            tmp16 = (uint16_t)*dest * (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, tmp16 > 0xFF);
            write_flag(cpu, FLAG_V, 0); // often V undefined for MUL
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            break;

        case VM_OP_DIV:
            if(src == 0) FAILED("Division by zero");
            result = *dest / src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_MOD:
            if(src == 0) FAILED("Modulo by zero");
            result = *dest % src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_AND:
            result = *dest & src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_OR:
            result = *dest | src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_XOR:
            result = *dest ^ src;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_NAND:
            result = ~(*dest & src);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_SHL:
            write_flag(cpu, FLAG_C, (*dest & 0x8000) != 0); // bit shifted out
            result = (*dest << 1) & 0xFF;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_SHR:
            write_flag(cpu, FLAG_C, (*dest & 0x01) != 0); // bit shifted out
            result = (*dest >> 1);
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, 0); // logical right shift clears N
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_INC:
            tmp16 = (uint16_t)*dest + 1;
            result = tmp16 & 0xFF;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, tmp16 > 0xFF);
            write_flag(cpu, FLAG_V, ((*dest ^ 1) & 0x8000) == 0 && ((*dest ^ result) & 0x8000) != 0);
            break;

        case VM_OP_DEC:
            tmp16 = (uint16_t)*dest - 1;
            result = tmp16 & 0xFF;
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, *dest >= 1); // borrow convention
            write_flag(cpu, FLAG_V, ((*dest ^ 1) & 0x8000) != 0 && ((*dest ^ result) & 0x8000) != 0);
            break;

        case VM_OP_NEG:
            result = (~(*dest) + 1) & 0xFF; // two’s complement negation
            *dest = result;
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            write_flag(cpu, FLAG_C, *dest != 0); // C = 1 if result != 0 (6502 style)
            write_flag(cpu, FLAG_V, *dest == 0x8000); // signed overflow if -128
            break;

        case VM_OP_MOV:
            *dest = src;
            write_flag(cpu, FLAG_Z, *dest == 0);
            write_flag(cpu, FLAG_N, *dest & 0x8000);
            write_flag(cpu, FLAG_C, 0);
            write_flag(cpu, FLAG_V, 0);
            break;

        case VM_OP_CMP:
            tmp16 = (uint16_t)*dest - (uint16_t)src;
            result = tmp16 & 0xFF;
            write_flag(cpu, FLAG_C, *dest >= src);
            write_flag(cpu, FLAG_V, ((*dest ^ src) & 0x8000) != 0 && ((*dest ^ result) & 0x8000) != 0);
            write_flag(cpu, FLAG_Z, result == 0);
            write_flag(cpu, FLAG_N, result & 0x8000);
            break;

        default: FAILED("Unsupported op %02x", op);
    }
}

static inline const char *vm_op_to_str(uint8_t op) {
    switch (op) {
        case VM_OP_NOP:  return "NOP";
        case VM_OP_ADD:  return "ADD";
        case VM_OP_SUB:  return "SUB";
        case VM_OP_MOV:  return "MOV";
        case VM_OP_CMP:  return "CMP";
        case VM_OP_JMP:  return "JMP";
        case VM_OP_AND:  return "AND";
        case VM_OP_OR:   return "OR";
        case VM_OP_NAND: return "NAND";
        case VM_OP_XOR:  return "XOR";
        case VM_OP_SHL:  return "SHL";
        case VM_OP_SHR:  return "SHR";
        case VM_OP_MUL:  return "MUL";
        case VM_OP_DIV:  return "DIV";
        case VM_OP_PUSH: return "PUSH";
        case VM_OP_POP:  return "POP";
        case VM_OP_MOD:  return "MOD";
        case VM_OP_INC:  return "INC";
        case VM_OP_DEC:  return "DEC";
        case VM_OP_NEG:  return "NEG";
        case VM_OP_JNE:  return "JNE";
        case VM_OP_JG:   return "JG";
        case VM_OP_JL:   return "JL";
        case VM_OP_JGE:  return "JGE";
        case VM_OP_JLE:  return "JLE";
        case VM_OP_JZ:   return "JZ";
        case VM_OP_RET:  return "RET";
        case VM_OP_HLT:  return "HLT";
        default:        FAILED("unknown %d", op); return "UNKNOWN";
    }
}


static inline int run(char* buf, size_t len) {
    cpu8_t* cpu = cpu8_create();
    memcpy(cpu->memory,buf, len);
    uint8_t c;
    uint8_t on = 1;
    #define i cpu->pc
    while (i < len && on) {
        c = cpu->memory[i++];
        uint8_t op = c>>3;
        printf("Instruction %s ", vm_op_to_str(op));
        switch (op) {
            case VM_OP_NOP: printf("\n"); break;
            case VM_OP_HLT: Info("VM_OP_HLT\n");on = 0; break;
            case VM_OP_ADD:
            case VM_OP_SUB:
            case VM_OP_MUL:
            case VM_OP_DIV:
            case VM_OP_MOD:
            case VM_OP_AND:
            case VM_OP_OR:
            case VM_OP_XOR:
            case VM_OP_NAND:
            case VM_OP_SHL:
            case VM_OP_SHR:
            case VM_OP_INC:
            case VM_OP_DEC:
            case VM_OP_NEG:
            case VM_OP_MOV:
            case VM_OP_CMP: { // dest, src -> dest = dest + src
                uint8_t type = c & 0b0000111;
                uint16_t* dest;
                uint16_t src;
                switch (type) {
                    case VM_OP_REG_IM: {
                        uint8_t reg = cpu->memory[i++];
                        uint8_t value_1 = cpu->memory[i++];
                        printf("reg %d the value %d\n", reg, value_1);

                        if (reg == RPC) {
                            uint8_t value_2 = cpu->memory[i++];
                            printf("actually: %d\n", (uint16_t)((value_2 << 8)+ value_1));
                            do_op_16(cpu, op, &cpu->pc, (uint16_t)((value_2 << 8) + value_1));
                            break;
                        }
                        if (reg == RSP) {
                            uint8_t value_2 = cpu->memory[i++];
                            printf("actually: %d\n", (uint16_t)((value_2 << 8)+ value_1));
                            do_op_16(cpu, op, &cpu->sp, (uint16_t)((value_2 << 8) + value_1));
                            break;
                        }
                        do_op(cpu, op, &cpu->reg[reg], value_1);
                        break;
                    }
                    case VM_OP_REG_REG: {
                        uint8_t regs = cpu->memory[i++];
                        uint8_t r1 = regs >> 4;
                        uint8_t r2 = regs & 0xf;
                        printf("reg %d value in reg %d\n", r1, r2);
                        do_op(cpu, op, &cpu->reg[r1], cpu->reg[r2]);
                        break;
                    }
                    case VM_OP_MEM_REG: { // dest, src
                        uint8_t mem_1 = cpu->memory[i++];
                        uint8_t mem_2 = cpu->memory[i++];
                        uint8_t reg = cpu->memory[i++];
                        printf("mem %d value in reg %d\n", (mem_2 << 8) + mem_1, reg);
                        if (reg == RPC) {
                            printf(" vaoue at mem = %u\n", *(uint16_t*)&cpu->memory[(mem_2 << 8) + mem_1 ]);
                            do_op_16(cpu, op, ((uint16_t*)&cpu->memory[(mem_2 << 8) + mem_1 ]), cpu->pc);
                            break;
                        }
                        if (reg == RSP) {
                            printf(" vaoue at mem = %u\n", *(uint16_t*)&cpu->memory[(mem_2 << 8) + mem_1 ]);
                            do_op_16(cpu, op, ((uint16_t*)&cpu->memory[(mem_2 << 8) + mem_1 ]), cpu->sp);
                            break;
                        }

                        do_op(cpu, op, &cpu->memory[(mem_2 << 8) + mem_1 ],
                              cpu->reg[reg]);
                        break;
                    }case VM_OP_REG_MEM: { // dest, src
                        uint8_t reg = cpu->memory[i++];
                        uint8_t mem_1 = cpu->memory[i++];
                        uint8_t mem_2 = cpu->memory[i++];
                        printf("reg %d value in mem %d\n",
                               reg,  (mem_2 << 8) + mem_1);
                        printf("mem %d value in reg %d\n",
                               (mem_2 << 8) + mem_1, reg);
                        if (reg == RPC) {
                            uint8_t least = cpu->memory[(mem_2 << 8) + mem_1];
                            uint8_t most = cpu->memory[(mem_2 << 8) + mem_1 + 1];
                            uint16_t value = (most<<8) | least;
                            printf(" value at mem = %u most least = %u %u\n",value, most, least);
                            do_op_16(cpu, op, &cpu->pc, *(uint16_t*)&cpu->memory[(mem_2 << 8) + mem_1 ]);
                            printf("New PC: %u\n", (uint16_t)cpu->pc);
                            break;
                        }
                        if (reg == RSP) {
                            uint8_t least = cpu->memory[(mem_2 << 8) + mem_1];
                            uint8_t most = cpu->memory[(mem_2 << 8) + mem_1 + 1];
                            uint16_t value = (most<<8) | least;
                            printf(" value at mem = %u most least = %u %u\n",value, most, least);
                            do_op_16(cpu, op, &cpu->sp, value);
                            printf("New SP: %u\n", (uint16_t)cpu->sp);
                            break;
                        }
                        do_op(cpu, op, &cpu->reg[reg],
                              cpu->memory[(mem_2 << 8) + mem_1 ]);
                        break;
                    }
                    case VM_OP_MEM_MEM: { // dest, src
                        uint8_t mem_1 = cpu->memory[i++];
                        uint8_t mem_2 = cpu->memory[i++];
                        uint8_t mem_21 = cpu->memory[i++];
                        uint8_t mem_22 = cpu->memory[i++];
                        printf("mem %d value in mem %d\n", (mem_2 << 8) + mem_1, (mem_22 << 8) + mem_21);
                        do_op(cpu, op, &cpu->memory[(mem_2 << 8) + mem_1 ],
                              cpu->memory[(mem_22 << 8) + mem_21 ]);
                        break;
                    }
                    case VM_OP_PC_MEM: {
                        uint8_t mem_1 = cpu->memory[i++];
                        uint8_t mem_2 = cpu->memory[i++];
                        printf("to pc value in mem %d\n", (mem_2 << 8) + mem_1);
                        do_op_16(cpu, op, &cpu->pc,
                              cpu->memory[(mem_2 << 8) + mem_1 ]);
                        break;
                    }
                    case VM_OP_MEM_PC: {
                        uint8_t mem_1 = cpu->memory[i++];
                        uint8_t mem_2 = cpu->memory[i++];
                        uint16_t t;
                        printf(" mem %d value in pc\n", (mem_2 << 8) + mem_1);
                        do_op_16(cpu, op, &t,
                              cpu->pc);
                        cpu->memory[(mem_2 << 8) + mem_1] = (uint8_t)(t & 0xff);
                        cpu->memory[(mem_2 << 8) + mem_1 + 1] = (uint8_t)(t >> 8);
                        break;
                    }
                    default: FAILED("Invalid VM_OP_TYPE: %u", type);
                };
                break;
            }
            // case VM_OP_JZ: { // Z == 1
            case VM_OP_JE: { // Z == 1
                uint8_t mem_1 = cpu->memory[i++];
                uint8_t mem_2 = cpu->memory[i++];
                if (test_flag(cpu,FLAG_Z)) 
                    cpu->pc = (mem_2 << 8) + mem_1 ;
                printf("\n");
                break;
            }
            case VM_OP_JMP: {
                uint8_t mem_1 = cpu->memory[i++];
                uint8_t mem_2 = cpu->memory[i++];
                cpu->pc = (mem_2 << 8) + mem_1 ;
                printf("\n");
                break;
            }
            case VM_OP_JNE: { // Z == 0
                uint8_t mem_1 = cpu->memory[i++];
                uint8_t mem_2 = cpu->memory[i++];
                if (!test_flag(cpu,FLAG_Z)) 
                    cpu->pc = (mem_2 << 8) + mem_1 ;
                printf("\n");
                break;
            }
            case VM_OP_JG: { // Z == 0 AND N == V
                uint8_t mem_1 = cpu->memory[i++];
                uint8_t mem_2 = cpu->memory[i++];
                if (!test_flag(cpu,FLAG_Z) && test_flag(cpu,FLAG_N) == test_flag(cpu,FLAG_V))
                    cpu->pc = (mem_2 << 8) + mem_1 ;
                printf("\n");
                break;
            }
            case VM_OP_JL: { // N != V
                uint8_t mem_1 = cpu->memory[i++];
                uint8_t mem_2 = cpu->memory[i++];
                if (test_flag(cpu,FLAG_N) != test_flag(cpu,FLAG_V)) 
                    cpu->pc = (mem_2 << 8) + mem_1 ;
                printf("\n");
                break;
            }
            case VM_OP_JGE: { // N == V
                uint8_t mem_1 = cpu->memory[i++];
                uint8_t mem_2 = cpu->memory[i++];
                if (test_flag(cpu,FLAG_N) == test_flag(cpu,FLAG_V)) 
                    cpu->pc = (mem_2 << 8) + mem_1 ;
                printf("\n");
                break;
            }
            case VM_OP_JLE: { // Z == 1 OR N != V
                uint8_t mem_1 = cpu->memory[i++];
                uint8_t mem_2 = cpu->memory[i++];
                if (test_flag(cpu,FLAG_N) == test_flag(cpu,FLAG_V) || test_flag(cpu,FLAG_Z)) 
                    cpu->pc = (mem_2 << 8) + mem_1 ;
                printf("\n");
                break;
            }
            case VM_OP_PUSH: {
                uint8_t reg = cpu->memory[i++];
                cpu->sp--;
                cpu->memory[cpu->sp] =  cpu->reg[reg];
                printf("\n");
                break;
            }
            case VM_OP_POP: {
                uint8_t reg = cpu->memory[i++];
                cpu->reg[reg] = cpu->memory[cpu->sp];
                cpu->sp++;
                printf("\n");
                break;
            }
            default: FAILED("Invalid operation: %u hlt=%u at: %u", c, VM_OP_HLT, i);
        }
    }
    int retval = cpu->reg[R0];
    cpu8_destroy(cpu);
    return retval;
}
#undef i




#endif // VM_H
