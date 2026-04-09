#pragma once
#include <stdint.h>
#include <stddef.h>
// ─── Opcodes ────────────────────────────────────────────────────────────────
typedef enum {
    OP_NONE = 0,
    // constants
    OP_PUSH_CONST_I, // pushes 4 bytes integer
    OP_PUSH_CONST_F, // pushes 4 bytes float

    // load pops 4 from stack for ptr
    OP_LOAD_4,  // load 4 bytes, integer/float/anything else 4 bytes
    // store pops 4 from stack for ptr and 4 more for value
    OP_STORE_4, // store 4 bytes, integer/float/anything else 4 bytes
    // load_off pops 4 from stack for ptr and 4 more for offset
    OP_LOAD_4_OFF,  // load 4 bytes, integer/float/anything else 4 bytes
    // store_off pops 4 from stack for ptr, 4 for offset and 4 more for value
    OP_STORE_4_OFF, // store 4 bytes, integer/float/anything else 4 bytes

    // pop 4 for value a, pop 4 for b, push a op b
    OP_ADDI, OP_SUBI, OP_MLTI, OP_DIVI,
    OP_ADDF, OP_SUBF, OP_MLTF, OP_DIVF,
    // pop 4 for a, pop 4 for b, push a == b
    OP_CMPI, OP_CMPF,
    // unconditional pops 0, JPT pops 4 and chacks if not 0, JP0 pops 4 jmp on 0
    OP_JMP, OP_JPT, OP_JP0,
    OP_HLT, // pops 4 and returns to vm
} OpCode;

typedef struct {
    OpCode op;
    int32_t operand; // meaning depends on opcode
} Instr;

// ─── Bytecode chunk (one per function) ──────────────────────────────────────

typedef struct {
    Instr*   code;
    int      code_len, code_cap;
    int      const_len, const_cap;

    int      local_count; // number of local slots needed
    char*    name;        // for debug
} Chunk;

#define VM_MEM (1 << 20)
#define VM_STACK_START (VM_MEM - 4096) // 4KB stack at top

typedef struct {
    char mem[VM_MEM];
    uint32_t pc;
    uint32_t sp;
    int had_error;
    char error[256];
} VM;


// vm
VM      vm_new(int stack_kb);
void    vm_free(VM* vm);
int     vm_run(VM* vm, int fn_index);     // run a function, return its value
void    vm_disasm(VM* vm, int fn_index);  // print disassembly
