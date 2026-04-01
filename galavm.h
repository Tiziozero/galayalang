#pragma once
#include <stdint.h>
#include <stddef.h>
#include "parser.h"

// ─── Opcodes ────────────────────────────────────────────────────────────────

typedef enum {
    // constants
    OP_CONST_I,     // push integer constant  | operand: pool index
    OP_CONST_F,     // push float constant    | operand: pool index
    OP_CONST_VOID,  // push void sentinel

    // locals
    OP_LOAD,        // push local[operand]
    OP_STORE,       // pop -> local[operand]
    OP_LOAD_ADDR,   // push &local[operand]  (pointer to stack slot)

    // globals
    OP_LOAD_GLOBAL,
    OP_STORE_GLOBAL,

    // arithmetic – integer
    OP_IADD, OP_ISUB, OP_IMUL, OP_IDIV, OP_IMOD,
    OP_INEG,
    // arithmetic – float
    OP_FADD, OP_FSUB, OP_FMUL, OP_FDIV,
    OP_FNEG,

    // comparison (result is i64 0/1)
    OP_IEQ, OP_INE, OP_ILT, OP_ILE, OP_IGT, OP_IGE,
    OP_FEQ, OP_FNE, OP_FLT, OP_FLE, OP_FGT, OP_FGE,

    // bitwise
    OP_AND, OP_OR, OP_XOR, OP_NOT, OP_SHL, OP_SHR,

    // casts
    OP_I2F,   // int -> float
    OP_F2I,   // float -> int (truncate)
    OP_WIDEN, // zero-extend to i64 (for smaller int types)
    OP_TRUNC, // truncate i64 to N bits | operand: bits

    // control flow
    OP_JMP,       // unconditional | operand: target ip
    OP_JZ,        // jump if top == 0
    OP_JNZ,       // jump if top != 0

    // functions
    OP_CALL,      // operand: fn index in fn table
    OP_CALL_PTR,  // top of stack is fn ptr (index)
    OP_RET,       // return top of stack (or void)
    OP_RET_VOID,

    // structs (heap allocated, referenced by pointer = heap index)
    OP_STRUCT_NEW,  // operand: struct type index -> push heap ptr
    OP_FIELD_GET,   // pop ptr, push ptr->fields[operand]
    OP_FIELD_SET,   // pop value, pop ptr -> ptr->fields[operand] = value

    // pointers / derefs
    OP_DEREF,       // pop ptr, push *ptr
    OP_STORE_PTR,   // pop value, pop ptr -> *ptr = value

    // stack
    OP_POP,
    OP_DUP,

    OP_HALT,
} OpCode;

// ─── Value ──────────────────────────────────────────────────────────────────

typedef enum {
    VAL_I64,
    VAL_F64,
    VAL_PTR,    // index into heap
    VAL_FN,     // index into fn table
    VAL_VOID,
} ValKind;

typedef struct {
    ValKind kind;
    union {
        int64_t  i;
        double   f;
        uint32_t ptr;  // heap index
        uint32_t fn;   // fn table index
    };
} Value;

#define VAL_I(x)    ((Value){.kind=VAL_I64, .i=(x)})
#define VAL_F(x)    ((Value){.kind=VAL_F64, .f=(x)})
#define VAL_PTR_(x) ((Value){.kind=VAL_PTR, .ptr=(x)})
#define VAL_FN_(x)  ((Value){.kind=VAL_FN,  .fn=(x)})
#define VAL_VOID_   ((Value){.kind=VAL_VOID})

// ─── Instruction ────────────────────────────────────────────────────────────

typedef struct {
    OpCode op;
    int32_t operand; // meaning depends on opcode
} Instr;

// ─── Bytecode chunk (one per function) ──────────────────────────────────────

typedef struct {
    Instr*   code;
    int      code_len, code_cap;

    Value*   const_pool;
    int      const_len, const_cap;

    int      local_count; // number of local slots needed
    char*    name;        // for debug
    Arena a;
} Chunk;

// ─── Heap object (struct instance) ──────────────────────────────────────────

typedef struct {
    Value*   fields;
    int      field_count;
    uint32_t type_id; // struct type index, for debug/gc later
} HeapObj;

// ─── VM ─────────────────────────────────────────────────────────────────────

#define VM_STACK_MAX  4096
#define VM_FRAMES_MAX 256
#define VM_HEAP_MAX   65536

typedef struct {
    Chunk*   chunk;
    int      ip;
    Value*   locals; // pointer into the locals array
    int      local_base; // base index into vm->locals_store
} CallFrame;

typedef struct {
    // execution state
    Value      stack[VM_STACK_MAX];
    int        sp; // stack pointer

    CallFrame  frames[VM_FRAMES_MAX];
    int        fc; // frame count

    // locals storage (all frames share one flat array)
    Value      locals_store[VM_STACK_MAX];
    int        locals_top;

    // heap
    HeapObj    heap[VM_HEAP_MAX];
    int        heap_top;

    // function table
    Chunk**    fns;
    int        fn_count, fn_cap;

    // globals
    Value*     globals;
    int        global_count;

    // error
    char       error[256];
    int        had_error;
} VM;

// ─── Public API ─────────────────────────────────────────────────────────────

// chunk
Chunk* chunk_new(const char* name);
void   chunk_free(Chunk* c);
int    chunk_emit(Chunk* c, OpCode op, int32_t operand);
int    chunk_add_const_i(Chunk* c, int64_t v);
int    chunk_add_const_f(Chunk* c, double v);
void   chunk_patch(Chunk* c, int idx, int32_t operand);

// vm
VM*    vm_new(int global_count);
void   vm_free(VM* vm);
int    vm_add_fn(VM* vm, Chunk* chunk);  // returns fn index
Value  vm_run(VM* vm, int fn_index);     // run a function, return its value
void   vm_disasm(VM* vm, int fn_index);  // print disassembly
int vm_get_fn_index(VM* vm, const char* name);
int cg_program(VM* vm, Node* root);
