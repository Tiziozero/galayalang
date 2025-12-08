// emu2.c
// Enhanced 64-bit-word emulator.
// Instruction word: uint64_t
//  - upper 32 bits: opcode (uint32_t)
//  - lower 32 bits: operand word (uint32_t) encoded as described below
//
// Registers: uint64_t regs[16] (R0..R15). R15 is SP (stack pointer).
// Memory: configurable, byte-addressable.
// Syscalls: SYSCALL uses host syscall() with regs[0] = syscall number, regs[1..6] args and stores return in regs[0].
//
// Addressing modes and instruction set are implemented with convenience helpers.

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/syscall.h>

// -------------------- Configuration --------------------
#define NUM_REGS 16
#define REG_SP 15   // R15 used as stack pointer
#define REG_BP 14   // optional base/frame pointer

#define MEM_SIZE (64 * 1024) // 64 KiB of emulated RAM (change as desired)

// -------------------- Opcodes (numeric) --------------------
#define OP_HALT      0x00
#define OP_MOV       0x01  // generic move (supports addressing modes)
#define OP_LOAD      0x02  // load from memory to reg
#define OP_STORE     0x03  // store reg -> memory
#define OP_ADD       0x10
#define OP_SUB       0x11
#define OP_MUL       0x12
#define OP_DIV       0x13
#define OP_AND       0x14
#define OP_OR        0x15
#define OP_XOR       0x16
#define OP_SHL       0x17
#define OP_SHR       0x18
#define OP_NEG       0x19
#define OP_INC       0x1A
#define OP_DEC       0x1B

#define OP_CMP       0x20  // compare sets flags (reg A vs reg B or imm)
#define OP_JMP       0x30
#define OP_JEQ       0x31
#define OP_JNE       0x32
#define OP_JLT       0x33
#define OP_JGT       0x34
#define OP_JLE       0x35
#define OP_JGE       0x36
#define OP_CALL      0x40
#define OP_RET       0x41
#define OP_PUSH      0x42
#define OP_POP       0x43

#define OP_SYSCALL   0xF0
#define OP_DEBUG     0xFE

// -------------------- Mnemonic strings (as requested) --------------------
#define OP_HALT_MN    "halt"
#define OP_MOV_MN     "mov"
#define OP_LOAD_MN    "load"
#define OP_STORE_MN   "store"
#define OP_ADD_MN     "add"
#define OP_SUB_MN     "sub"
#define OP_MUL_MN     "mul"
#define OP_DIV_MN     "div"
#define OP_AND_MN     "and"
#define OP_OR_MN      "or"
#define OP_XOR_MN     "xor"
#define OP_SHL_MN     "shl"
#define OP_SHR_MN     "shr"
#define OP_NEG_MN     "neg"
#define OP_INC_MN     "inc"
#define OP_DEC_MN     "dec"
#define OP_CMP_MN     "cmp"
#define OP_JMP_MN     "jmp"
#define OP_JEQ_MN     "jeq"
#define OP_JNE_MN     "jne"
#define OP_JLT_MN     "jlt"
#define OP_JGT_MN     "jgt"
#define OP_JLE_MN     "jle"
#define OP_JGE_MN     "jge"
#define OP_CALL_MN    "call"
#define OP_RET_MN     "ret"
#define OP_PUSH_MN    "push"
#define OP_POP_MN     "pop"
#define OP_SYSCALL_MN "syscall"
#define OP_DEBUG_MN   "debug"

// -------------------- Addressing modes (4 bits in operand high nibble) --------------------
#define AM_REG       0   // register direct
#define AM_IMM       1   // immediate contained in operand (imm20) or next word if AM_ABS64
#define AM_REG_IND   2   // memory at [reg]
#define AM_BASE_OFF  3   // memory at [reg + imm16] (signed imm16)
#define AM_ABS64     4   // 64-bit immediate/address in next word

// -------------------- Operand helpers --------------------
// operand (32 bit) layout:
// bits 31..28 = mode (4 bits)
// bits 27..24 = dst reg (4 bits)
// bits 23..20 = src reg (4 bits)
// bits 19..0  = imm20 (20 bits) - can encode small immediates or low addr bits
static inline uint32_t OP_MODE(uint32_t op) { return (op >> 28) & 0xF; }
static inline uint32_t OP_DSTREG(uint32_t op) { return (op >> 24) & 0xF; }
static inline uint32_t OP_SRCREG(uint32_t op) { return (op >> 20) & 0xF; }
static inline uint32_t OP_IMM20(uint32_t op) { return op & 0xFFFFF; } // 20-bit

// sign-extend a 16-bit offset stored in lower 16 bits of imm20
static inline int64_t SIGN_EXTEND_16(uint32_t imm20) {
    uint16_t low16 = imm20 & 0xFFFF;
    int16_t s = (int16_t)low16;
    return (int64_t)s;
}

// sign-extend 20-bit immediate
static inline int64_t SIGN_EXTEND_20(uint32_t imm20) {
    if (imm20 & (1u << 19)) { // negative
        return (int64_t)(imm20 | 0xFFF00000u); // fill upper bits
    } else {
        return (int64_t)imm20;
    }
}

// -------------------- Flags --------------------
typedef struct {
    uint8_t Z; // zero
    uint8_t S; // sign
    uint8_t C; // carry (simple support)
} flags_t;

// -------------------- Emulator state --------------------
typedef struct {
    uint64_t regs[NUM_REGS];
    flags_t flags;
    uint8_t *mem;
    size_t mem_size;
} vm_t;

// Helper: check memory access
static int mem_check(vm_t *vm, uint64_t addr, size_t bytes) {
    if (addr + bytes > vm->mem_size) return 0;
    return 1;
}

// read/write little-endian helpers for emulated memory
static uint64_t mem_read64(vm_t *vm, uint64_t addr) {
    if (!mem_check(vm, addr, 8)) {
        fprintf(stderr, "mem_read64: OOB addr=0x%016" PRIx64 "\n", addr);
        return 0;
    }
    uint64_t v;
    memcpy(&v, vm->mem + addr, 8);
    return v;
}
static void mem_write64(vm_t *vm, uint64_t addr, uint64_t val) {
    if (!mem_check(vm, addr, 8)) {
        fprintf(stderr, "mem_write64: OOB addr=0x%016" PRIx64 "\n", addr);
        return;
    }
    memcpy(vm->mem + addr, &val, 8);
}
static uint32_t mem_read32(vm_t *vm, uint64_t addr) {
    if (!mem_check(vm, addr, 4)) {
        fprintf(stderr, "mem_read32: OOB addr=0x%016" PRIx64 "\n", addr);
        return 0;
    }
    uint32_t v;
    memcpy(&v, vm->mem + addr, 4);
    return v;
}
static void mem_write32(vm_t *vm, uint64_t addr, uint32_t val) {
    if (!mem_check(vm, addr, 4)) {
        fprintf(stderr, "mem_write32: OOB addr=0x%016" PRIx64 "\n", addr);
        return;
    }
    memcpy(vm->mem + addr, &val, 4);
}
static uint8_t mem_read8(vm_t *vm, uint64_t addr) {
    if (!mem_check(vm, addr, 1)) {
        fprintf(stderr, "mem_read8: OOB addr=0x%016" PRIx64 "\n", addr);
        return 0;
    }
    return vm->mem[addr];
}
static void mem_write8(vm_t *vm, uint64_t addr, uint8_t val) {
    if (!mem_check(vm, addr, 1)) {
        fprintf(stderr, "mem_write8: OOB addr=0x%016" PRIx64 "\n", addr);
        return;
    }
    vm->mem[addr] = val;
}

// Push/pop helper (64-bit)
static void push64(vm_t *vm, uint64_t val) {
    // stack grows downwards: SP points to next free (conventional)
    vm->regs[REG_SP] -= 8;
    if (!mem_check(vm, vm->regs[REG_SP], 8)) {
        fprintf(stderr, "stack overflow/underflow in push\n");
        return;
    }
    mem_write64(vm, vm->regs[REG_SP], val);
}
static uint64_t pop64(vm_t *vm) {
    if (!mem_check(vm, vm->regs[REG_SP], 8)) {
        fprintf(stderr, "stack pop OOB\n");
        return 0;
    }
    uint64_t v = mem_read64(vm, vm->regs[REG_SP]);
    vm->regs[REG_SP] += 8;
    return v;
}

// Set flags from a 64-bit result for CMP/ALU (simple)
static void set_flags_from_result(vm_t *vm, uint64_t res) {
    vm->flags.Z = (res == 0);
    vm->flags.S = ((int64_t)res < 0);
    // C left alone unless operation sets it explicitly (we may set during adds/subs)
}

// -------------------- Instruction execution --------------------
void run_emulator(const uint64_t *program, size_t prog_len, vm_t *vm) {
    size_t pc = 0;
    while (pc < prog_len) {
        uint64_t instr = program[pc];
        uint32_t opcode = (uint32_t)(instr >> 32);
        uint32_t operand = (uint32_t)(instr & 0xFFFFFFFFu);

        uint32_t mode = OP_MODE(operand);
        uint32_t dst = OP_DSTREG(operand) & 0xF;
        uint32_t src = OP_SRCREG(operand) & 0xF;
        uint32_t imm20 = OP_IMM20(operand);

        // helpers to obtain operand values depending on mode
        uint64_t val_dst = 0, val_src = 0;
        uint64_t addr = 0;
        int64_t sdisp = 0;

        // compute src value if needed
        if (mode == AM_REG) {
            val_src = vm->regs[src];
        } else if (mode == AM_IMM) {
            val_src = (uint64_t) SIGN_EXTEND_20(imm20);
        } else if (mode == AM_REG_IND) {
            addr = vm->regs[src];
            if (!mem_check(vm, addr, 8)) {
                fprintf(stderr, "OOB memory at [r%u]\n", src);
                return;
            }
            val_src = mem_read64(vm, addr);
        } else if (mode == AM_BASE_OFF) {
            sdisp = SIGN_EXTEND_16(imm20);
            addr = (uint64_t) ((int64_t)vm->regs[src] + sdisp);
            if (!mem_check(vm, addr, 8)) {
                fprintf(stderr, "OOB memory at [r%u + %ld]\n", src, (long)sdisp);
                return;
            }
            val_src = mem_read64(vm, addr);
        } else if (mode == AM_ABS64) {
            // immediate 64-bit in next program word
            if (pc + 1 >= prog_len) {
                fprintf(stderr, "ABS64 expected 64-bit immediate but none available (pc=%zu)\n", pc);
                return;
            }
            val_src = program[pc + 1];
        }

        switch (opcode) {
            case OP_HALT:
                return;

            case OP_MOV:
                // MOV semantics: move "source" to destination register or memory depending on mode+dst
                // For simplicity: dst is always a register when using these encodings; if dst reg is 0xF and mode indicates store, we could support memory writes.
                if (mode == AM_REG || mode == AM_IMM || mode == AM_ABS64) {
                    // val_src already computed (for ABS64 consumed next word)
                    vm->regs[dst] = val_src;
                    if (mode == AM_ABS64) pc += 2; else pc += 1;
                } else if (mode == AM_REG_IND) {
                    // store source register's value (src field indicates src reg) into memory pointed by dst reg?
                    // Here we'll interpret dst as register whose value is memory address to store into, and src is source reg.
                    uint64_t ea = vm->regs[dst];
                    if (!mem_check(vm, ea, 8)) { fprintf(stderr, "MOV store OOB\n"); return; }
                    mem_write64(vm, ea, vm->regs[src]);
                    pc += 1;
                } else if (mode == AM_BASE_OFF) {
                    // store src reg into [dst + off] where dst is base reg, off in imm16
                    int64_t off = SIGN_EXTEND_16(imm20);
                    uint64_t ea = (uint64_t)((int64_t)vm->regs[dst] + off);
                    if (!mem_check(vm, ea, 8)) { fprintf(stderr, "MOV store base+off OOB\n"); return; }
                    mem_write64(vm, ea, vm->regs[src]);
                    pc += 1;
                } else {
                    fprintf(stderr, "MOV: unsupported addressing mode %u\n", mode);
                    return;
                }
                break;

            case OP_LOAD:
                // LOAD loads memory into dst register. Interpret mode to compute source memory.
                if (mode == AM_REG_IND) {
                    addr = vm->regs[src];
                    if (!mem_check(vm, addr, 8)) { fprintf(stderr, "LOAD OOB\n"); return; }
                    vm->regs[dst] = mem_read64(vm, addr);
                    pc += 1;
                } else if (mode == AM_BASE_OFF) {
                    int64_t off = SIGN_EXTEND_16(imm20);
                    addr = (uint64_t)((int64_t)vm->regs[src] + off);
                    if (!mem_check(vm, addr, 8)) { fprintf(stderr, "LOAD base+off OOB\n"); return; }
                    vm->regs[dst] = mem_read64(vm, addr);
                    pc += 1;
                } else if (mode == AM_ABS64) {
                    if (pc + 1 >= prog_len) { fprintf(stderr, "LOAD needs next word addr\n"); return; }
                    addr = program[pc + 1];
                    if (!mem_check(vm, addr, 8)) { fprintf(stderr, "LOAD abs OOB\n"); return; }
                    vm->regs[dst] = mem_read64(vm, addr);
                    pc += 2;
                } else {
                    fprintf(stderr, "LOAD: unsupported mode %u\n", mode);
                    return;
                }
                break;

            case OP_STORE:
                // STORE store value of src reg to memory location described by mode+dst (dst is base reg usually)
                if (mode == AM_REG_IND) {
                    addr = vm->regs[dst];
                    if (!mem_check(vm, addr, 8)) { fprintf(stderr, "STORE OOB\n"); return; }
                    mem_write64(vm, addr, vm->regs[src]);
                    pc += 1;
                } else if (mode == AM_BASE_OFF) {
                    int64_t off = SIGN_EXTEND_16(imm20);
                    addr = (uint64_t)((int64_t)vm->regs[dst] + off);
                    if (!mem_check(vm, addr, 8)) { fprintf(stderr, "STORE base+off OOB\n"); return; }
                    mem_write64(vm, addr, vm->regs[src]);
                    pc += 1;
                } else if (mode == AM_ABS64) {
                    if (pc + 1 >= prog_len) { fprintf(stderr, "STORE needs next word addr\n"); return; }
                    addr = program[pc + 1];
                    if (!mem_check(vm, addr, 8)) { fprintf(stderr, "STORE abs OOB\n"); return; }
                    mem_write64(vm, addr, vm->regs[src]);
                    pc += 2;
                } else {
                    fprintf(stderr, "STORE: unsupported mode %u\n", mode);
                    return;
                }
                break;

            // Arithmetic / logical ops: semantics usually dst = dst op src
            case OP_ADD: {
                uint64_t a = vm->regs[dst];
                uint64_t b = (mode == AM_REG ? vm->regs[src] : (mode == AM_IMM ? (uint64_t)SIGN_EXTEND_20(imm20) : val_src));
                __uint128_t r = ( __uint128_t)a + (__uint128_t)b;
                vm->regs[dst] = (uint64_t)r;
                vm->flags.C = (r >> 64) != 0;
                set_flags_from_result(vm, vm->regs[dst]);
                if (mode == AM_ABS64) pc += 2; else pc += 1;
                break;
            }
            case OP_SUB: {
                uint64_t a = vm->regs[dst];
                uint64_t b = (mode == AM_REG ? vm->regs[src] : (mode == AM_IMM ? (uint64_t)SIGN_EXTEND_20(imm20) : val_src));
                __int128_t r = ( __int128_t)(int64_t)a - ( __int128_t)(int64_t)b;
                vm->regs[dst] = (uint64_t)r;
                vm->flags.C = (a < b);
                set_flags_from_result(vm, vm->regs[dst]);
                if (mode == AM_ABS64) pc += 2; else pc += 1;
                break;
            }
            case OP_MUL: {
                uint64_t a = vm->regs[dst];
                uint64_t b = (mode == AM_REG ? vm->regs[src] : (mode == AM_IMM ? (uint64_t)SIGN_EXTEND_20(imm20) : val_src));
                __uint128_t r = ( __uint128_t)a * (__uint128_t)b;
                vm->regs[dst] = (uint64_t)r;
                vm->flags.C = (r >> 64) != 0;
                set_flags_from_result(vm, vm->regs[dst]);
                if (mode == AM_ABS64) pc += 2; else pc += 1;
                break;
            }
            case OP_DIV: {
                uint64_t a = vm->regs[dst];
                uint64_t b = (mode == AM_REG ? vm->regs[src] : (mode == AM_IMM ? (uint64_t)SIGN_EXTEND_20(imm20) : val_src));
                if (b == 0) { fprintf(stderr, "DIV by zero\n"); return; }
                vm->regs[dst] = a / b;
                set_flags_from_result(vm, vm->regs[dst]);
                if (mode == AM_ABS64) pc += 2; else pc += 1;
                break;
            }
            case OP_AND:
                vm->regs[dst] &= (mode == AM_REG ? vm->regs[src] : (mode==AM_IMM?(uint64_t)SIGN_EXTEND_20(imm20):val_src));
                set_flags_from_result(vm, vm->regs[dst]); pc += (mode==AM_ABS64?2:1);
                break;
            case OP_OR:
                vm->regs[dst] |= (mode == AM_REG ? vm->regs[src] : (mode==AM_IMM?(uint64_t)SIGN_EXTEND_20(imm20):val_src));
                set_flags_from_result(vm, vm->regs[dst]); pc += (mode==AM_ABS64?2:1);
                break;
            case OP_XOR:
                vm->regs[dst] ^= (mode == AM_REG ? vm->regs[src] : (mode==AM_IMM?(uint64_t)SIGN_EXTEND_20(imm20):val_src));
                set_flags_from_result(vm, vm->regs[dst]); pc += (mode==AM_ABS64?2:1);
                break;
            case OP_SHL: {
                unsigned sh = (unsigned)((mode==AM_REG)? vm->regs[src] & 0x3F : (unsigned)imm20);
                vm->regs[dst] <<= sh;
                set_flags_from_result(vm, vm->regs[dst]); pc += (mode==AM_ABS64?2:1);
                break;
            }
            case OP_SHR: {
                unsigned sh = (unsigned)((mode==AM_REG)? vm->regs[src] & 0x3F : (unsigned)imm20);
                vm->regs[dst] >>= sh;
                set_flags_from_result(vm, vm->regs[dst]); pc += (mode==AM_ABS64?2:1);
                break;
            }
            case OP_NEG:
                vm->regs[dst] = (uint64_t)(-(int64_t)vm->regs[dst]);
                set_flags_from_result(vm, vm->regs[dst]); pc += 1;
                break;
            case OP_INC:
                vm->regs[dst] += 1; set_flags_from_result(vm, vm->regs[dst]); pc += 1; break;
            case OP_DEC:
                vm->regs[dst] -= 1; set_flags_from_result(vm, vm->regs[dst]); pc += 1; break;

            case OP_CMP: {
                // CMP regs[dst] and (reg/src/imm)
                uint64_t a = vm->regs[dst];
                uint64_t b = (mode == AM_REG ? vm->regs[src] : (mode==AM_IMM ? (uint64_t)SIGN_EXTEND_20(imm20) : val_src));
                int64_t ar = (int64_t)a;
                int64_t br = (int64_t)b;
                int64_t diff = ar - br;
                vm->flags.Z = (diff == 0);
                vm->flags.S = (diff < 0);
                vm->flags.C = (a < b);
                if (mode == AM_ABS64) pc += 2; else pc += 1;
                break;
            }

            // Jumps use the imm20 (or next word in ABS64) as the target PC (index into program array)
            case OP_JMP: {
                uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                if (mode == AM_ABS64) pc = (size_t)target; else pc = (size_t)target;
                break;
            }
            case OP_JEQ: {
                if (vm->flags.Z) {
                    uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                    if (mode == AM_ABS64) pc = (size_t)target; else pc = (size_t)target;
                } else {
                    pc += (mode==AM_ABS64?2:1);
                }
                break;
            }
            case OP_JNE: {
                if (!vm->flags.Z) {
                    uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                    pc = (size_t)target;
                } else {
                    pc += (mode==AM_ABS64?2:1);
                }
                break;
            }
            case OP_JLT: {
                if (vm->flags.S) {
                    uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                    pc = (size_t)target;
                } else pc += (mode==AM_ABS64?2:1);
                break;
            }
            case OP_JGT: {
                if (!vm->flags.Z && !vm->flags.S) {
                    uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                    pc = (size_t)target;
                } else pc += (mode==AM_ABS64?2:1);
                break;
            }
            case OP_JLE: {
                if (vm->flags.Z || vm->flags.S) {
                    uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                    pc = (size_t)target;
                } else pc += (mode==AM_ABS64?2:1);
                break;
            }
            case OP_JGE: {
                if (!vm->flags.S) {
                    uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                    pc = (size_t)target;
                } else pc += (mode==AM_ABS64?2:1);
                break;
            }

            case OP_CALL: {
                // CALL pushes return (pc+1 or +2 if ABS64) then jumps to target
                size_t nextpc = pc + (mode==AM_ABS64?2:1);
                uint64_t retaddr = (uint64_t)nextpc;
                push64(vm, retaddr);
                uint64_t target = (mode == AM_ABS64 ? program[pc+1] : (uint64_t)imm20);
                pc = (size_t)target;
                break;
            }
            case OP_RET: {
                uint64_t ret = pop64(vm);
                pc = (size_t)ret;
                break;
            }

            case OP_PUSH: {
                // push src (mode indicates reg/imm/abs)
                uint64_t v = 0;
                if (mode == AM_REG) v = vm->regs[src];
                else if (mode == AM_IMM) v = (uint64_t)SIGN_EXTEND_20(imm20);
                else if (mode == AM_ABS64) { if (pc+1>=prog_len){fprintf(stderr,"push abs missing immediate\n");return;} v = program[pc+1]; }
                push64(vm, v);
                pc += (mode==AM_ABS64?2:1);
                break;
            }
            case OP_POP: {
                // pop into dst register
                vm->regs[dst] = pop64(vm);
                pc += 1;
                break;
            }

            case OP_SYSCALL: {
                long num = (long)vm->regs[0];
                long a1 = (long)vm->regs[1];
                long a2 = (long)vm->regs[2];
                long a3 = (long)vm->regs[3];
                long a4 = (long)vm->regs[4];
                long a5 = (long)vm->regs[5];
                long a6 = (long)vm->regs[6];
                long ret = syscall(num, a1, a2, a3, a4, a5, a6);
                vm->regs[0] = (uint64_t)ret;
                pc += 1;
                break;
            }

            case OP_DEBUG: {
                printf("==== DEBUG PC=%zu ====\n", pc);
                for (int i = 0; i < NUM_REGS; ++i) {
                    printf(" R%-2d = 0x%016" PRIx64 " (%" PRIu64 ")\n", i, vm->regs[i], vm->regs[i]);
                }
                printf(" FLAGS: Z=%u S=%u C=%u\n", vm->flags.Z, vm->flags.S, vm->flags.C);
                // dump first 64 bytes of memory as example
                printf(" MEM[0..64]: ");
                for (int b = 0; b < 64 && b < (int)vm->mem_size; ++b) {
                    printf("%02x ", vm->mem[b]);
                }
                printf("\n==== END DEBUG ====\n");
                pc += 1;
                break;
            }

            default:
                fprintf(stderr, "Unknown opcode 0x%02x at pc=%zu\n", opcode, pc);
                return;
        }
    }
}

// -------------------- Helpers to construct instructions --------------------
static inline uint64_t instr(uint32_t opcode, uint32_t operand) {
    return ((uint64_t)opcode << 32) | (uint64_t)operand;
}

// convenience build functions for operand: mode,dst,src,imm20
static inline uint32_t build_operand(uint32_t mode, uint32_t dst, uint32_t src, uint32_t imm20) {
    return ((mode & 0xF) << 28) | ((dst & 0xF) << 24) | ((src & 0xF) << 20) | (imm20 & 0xFFFFF);
}

// -------------------- Demo program --------------------
// We'll write a short program that computes factorial(6) -> stores result in R0, then DEBUG.
// Program is an array of uint64_t words (instructions and inline immediates when needed).
int main(void) {
    // allocate VM
    vm_t vm;
    memset(&vm, 0, sizeof(vm));
    vm.mem_size = MEM_SIZE;
    vm.mem = calloc(1, vm.mem_size);
    if (!vm.mem) { perror("calloc"); return 1; }

    // initialize stack pointer somewhere near top of memory
    vm.regs[REG_SP] = vm.mem_size - 8; // align a bit

    // We'll create a program vector that uses the addressing modes above.
    // For readability: macros to build operand quickly:
    // mode, dst, src, imm20

    #define OP_REG(mode,dst,src) build_operand(mode,dst,src,0)
    #define OP_IMM20(dst,imm20) build_operand(AM_IMM, dst, 0, ((uint32_t)imm20) & 0xFFFFF)
    #define OP_ABS64(dst) build_operand(AM_ABS64, dst, 0, 0)

    uint64_t prog[256];
    size_t ip = 0;

    // We'll compute factorial:
    // R0 = n (6)
    // R1 = result (1)
    // LOOP:
    //   CMP R0, 1
    //   JLE END
    //   MUL R1, R0
    //   DEC R0
    //   JMP LOOP
    // END: MOV R0 <- R1 ; DEBUG ; HALT

    // MOV R0, imm 6
    prog[ip++] = instr(OP_MOV, build_operand(AM_ABS64, 0, 0, 0)); // next word is imm64
    prog[ip++] = 6;

    // MOV R1, imm 1
    prog[ip++] = instr(OP_MOV, build_operand(AM_ABS64, 1, 0, 0));
    prog[ip++] = 1;

    size_t loop_pc = ip;

    // CMP R0, imm 1   (mode AM_IMM)
    prog[ip++] = instr(OP_CMP, build_operand(AM_IMM, 0, 0, 1)); // compare R0 to imm1 (dst field holds reg to compare: dst=0)

    // JLE to end (we'll patch target later). For now set imm20 = placeholder
    size_t jle_index = ip;
    prog[ip++] = instr(OP_JLE, build_operand(AM_IMM, 0, 0, 0)); // immediate target will be patched

    // MUL R1, R0  (R1 *= R0) mode AM_REG (src in src field)
    prog[ip++] = instr(OP_MUL, build_operand(AM_REG, 1, 0, 0)); // dst=1, src=0

    // DEC R0
    prog[ip++] = instr(OP_DEC, build_operand(AM_REG, 0, 0, 0));

    // JMP to loop_pc (imm20)
    prog[ip++] = instr(OP_JMP, build_operand(AM_IMM, 0, 0, (uint32_t)loop_pc));

    // patch end target
    size_t end_pc = ip;

    // MOV R0, R1
    prog[ip++] = instr(OP_MOV, build_operand(AM_REG, 0, 1, 0));

    // DEBUG
    prog[ip++] = instr(OP_DEBUG, 0);

    // HALT
    prog[ip++] = instr(OP_HALT, 0);

    // now fix the JLE operand target to 'end_pc'
    // JLE instruction is at jle_index -> set its operand imm20 to end_pc
    uint32_t oldop = (uint32_t)(prog[jle_index] & 0xFFFFFFFFu);
    uint32_t newop = (oldop & 0xFFF00000u) | (end_pc & 0xFFFFFu); // keep mode/dst/src, set imm20
    prog[jle_index] = instr(OP_JLE, newop);

    size_t prog_len = ip;

    // Run emulator
    run_emulator(prog, prog_len, &vm);

    // print final R0
    printf("Final R0 (factorial result) = %" PRIu64 "\n", vm.regs[0]);

    free(vm.mem);
    return 0;
}

