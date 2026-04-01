#include "galavm.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// ─── Chunk ──────────────────────────────────────────────────────────────────

Chunk* chunk_new(const char* name) {
    Arena a = arena_new(10, 200);
    dbg("Adding chunk name %s", name);
    Chunk* c = calloc(1, sizeof(Chunk));
    c->a = a;
    if (name) {
        dbg("Duping name %s.", name);
    } else {
        dbg("name is ?.");
    }
    char* alloced = name ? arena_str_dup(&a, name) : arena_str_dup(&a, "?");
    dbg("Chung name ptr %zu ", alloced);
    dbg("Chung name name %s ", alloced);
    c->name = alloced;
    return c;
}

void chunk_free(Chunk* c) {
    free(c->code);
    free(c->const_pool);
    free(c->name);
    for (int i = 0; i < c->a.pages_count; i++) {
        free(c->a.pages[i]);
    }
    free(c->a.pages);
    free(c);
}

int chunk_emit(Chunk* c, OpCode op, int32_t operand) {
    if (c->code_len == c->code_cap) {
        c->code_cap = c->code_cap ? c->code_cap * 2 : 16;
        c->code = realloc(c->code, c->code_cap * sizeof(Instr));
    }
    c->code[c->code_len] = (Instr){op, operand};
    return c->code_len++;
}

static int chunk_add_const(Chunk* c, Value v) {
    if (c->const_len == c->const_cap) {
        c->const_cap = c->const_cap ? c->const_cap * 2 : 8;
        c->const_pool = realloc(c->const_pool, c->const_cap * sizeof(Value));
    }
    c->const_pool[c->const_len] = v;
    return c->const_len++;
}

int chunk_add_const_i(Chunk* c, int64_t v) { return chunk_add_const(c, VAL_I(v)); }
int chunk_add_const_f(Chunk* c, double  v) { return chunk_add_const(c, VAL_F(v)); }

void chunk_patch(Chunk* c, int idx, int32_t operand) {
    c->code[idx].operand = operand;
}

// ─── VM lifecycle ───────────────────────────────────────────────────────────

VM* vm_new(int global_count) {
    VM* vm = calloc(1, sizeof(VM));
    vm->global_count = global_count;
    if (global_count > 0)
        vm->globals = calloc(global_count, sizeof(Value));
    return vm;
}

void vm_free(VM* vm) {
    for (int i = 0; i < vm->heap_top; i++)
        free(vm->heap[i].fields);
    for (int i = 0; i < vm->fn_count; i++)
        chunk_free(vm->fns[i]);
    free(vm->fns);
    free(vm->globals);
    free(vm);
}

int vm_add_fn(VM* vm, Chunk* chunk) {
    if (vm->fn_count == vm->fn_cap) {
        vm->fn_cap = vm->fn_cap ? vm->fn_cap * 2 : 8;
        vm->fns = realloc(vm->fns, vm->fn_cap * sizeof(Chunk*));
    }
    dbg("Fn %s", chunk->name);
    vm->fns[vm->fn_count] = chunk;
    return vm->fn_count++;
}

// ─── Stack helpers ──────────────────────────────────────────────────────────

static inline void push(VM* vm, Value v) {
    assert(vm->sp < VM_STACK_MAX && "stack overflow");
    vm->stack[vm->sp++] = v;
}

static inline Value pop(VM* vm) {
    assert(vm->sp > 0 && "stack underflow");
    return vm->stack[--vm->sp];
}

static inline Value peek_vm(VM* vm, int offset) {
    return vm->stack[vm->sp - 1 - offset];
}

// ─── Heap helpers ───────────────────────────────────────────────────────────

static uint32_t heap_alloc(VM* vm, int field_count, uint32_t type_id) {
    assert(vm->heap_top < VM_HEAP_MAX && "heap overflow");
    int idx = vm->heap_top++;
    vm->heap[idx].fields = calloc(field_count, sizeof(Value));
    vm->heap[idx].field_count = field_count;
    vm->heap[idx].type_id = type_id;
    return (uint32_t)idx;
}

// ─── Error ──────────────────────────────────────────────────────────────────

#define VM_ERR(vm, ...) do { \
    snprintf((vm)->error, sizeof((vm)->error), __VA_ARGS__); \
    (vm)->had_error = 1; \
} while(0)

// ─── Type coercion helpers ──────────────────────────────────────────────────

// For binops: if either side is float, promote both
static inline int either_float(Value a, Value b) {
    return a.kind == VAL_F64 || b.kind == VAL_F64;
}
static inline double to_f(Value v) {
    return v.kind == VAL_F64 ? v.f : (double)v.i;
}
static inline int64_t to_i(Value v) {
    return v.kind == VAL_I64 ? v.i : (int64_t)v.f;
}

// ─── Execution ──────────────────────────────────────────────────────────────

Value vm_run(VM* vm, int fn_index) {
    assert(fn_index >= 0 && fn_index < vm->fn_count);

    // push initial frame
    CallFrame* frame = &vm->frames[vm->fc++];
    frame->chunk      = vm->fns[fn_index];
    frame->ip         = 0;
    frame->local_base = vm->locals_top;
    vm->locals_top   += frame->chunk->local_count;
    frame->locals     = vm->locals_store + frame->local_base;
    // zero locals
    memset(frame->locals, 0, frame->chunk->local_count * sizeof(Value));

    #define CHUNK  frame->chunk
    #define CODE   frame->chunk->code
    #define IP     frame->ip
    #define POOL   frame->chunk->const_pool
    #define LOCAL  frame->locals

    while (!vm->had_error) {
        Instr instr = CODE[IP++];
        OpCode op   = instr.op;
        int32_t arg = instr.operand;

        switch (op) {

        // ── constants ──────────────────────────────────────────────────────
        case OP_CONST_I: push(vm, POOL[arg]); break;
        case OP_CONST_F: push(vm, POOL[arg]); break;
        case OP_CONST_VOID: push(vm, VAL_VOID_); break;

        // ── locals ─────────────────────────────────────────────────────────
        case OP_LOAD:      push(vm, LOCAL[arg]); break;
        case OP_STORE:     LOCAL[arg] = pop(vm); break;
        case OP_LOAD_ADDR: {
            // We model a pointer-to-local as a special PTR with a magic tag.
            // In a real impl you'd use stable stack addresses; here we store
            // the absolute locals_store index so deref works.
            uint32_t abs = (uint32_t)(frame->local_base + arg);
            push(vm, VAL_PTR_(abs | 0x80000000u)); // high bit = "local ptr"
        } break;

        // ── globals ────────────────────────────────────────────────────────
        case OP_LOAD_GLOBAL:  push(vm, vm->globals[arg]); break;
        case OP_STORE_GLOBAL: vm->globals[arg] = pop(vm); break;

        // ── integer arithmetic ─────────────────────────────────────────────
        case OP_IADD: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i+b.i)); } break;
        case OP_ISUB: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i-b.i)); } break;
        case OP_IMUL: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i*b.i)); } break;
        case OP_IDIV: { Value b=pop(vm),a=pop(vm);
            if (!b.i) { VM_ERR(vm,"div by zero"); break; }
            push(vm,VAL_I(a.i/b.i)); } break;
        case OP_IMOD: { Value b=pop(vm),a=pop(vm);
            if (!b.i) { VM_ERR(vm,"mod by zero"); break; }
            push(vm,VAL_I(a.i%b.i)); } break;
        case OP_INEG: { Value a=pop(vm); push(vm,VAL_I(-a.i)); } break;

        // ── float arithmetic ───────────────────────────────────────────────
        case OP_FADD: { Value b=pop(vm),a=pop(vm); push(vm,VAL_F(to_f(a)+to_f(b))); } break;
        case OP_FSUB: { Value b=pop(vm),a=pop(vm); push(vm,VAL_F(to_f(a)-to_f(b))); } break;
        case OP_FMUL: { Value b=pop(vm),a=pop(vm); push(vm,VAL_F(to_f(a)*to_f(b))); } break;
        case OP_FDIV: { Value b=pop(vm),a=pop(vm); push(vm,VAL_F(to_f(a)/to_f(b))); } break;
        case OP_FNEG: { Value a=pop(vm); push(vm,VAL_F(-to_f(a))); } break;

        // ── integer compare ────────────────────────────────────────────────
        case OP_IEQ: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i==b.i)); } break;
        case OP_INE: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i!=b.i)); } break;
        case OP_ILT: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i< b.i)); } break;
        case OP_ILE: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i<=b.i)); } break;
        case OP_IGT: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i> b.i)); } break;
        case OP_IGE: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i>=b.i)); } break;

        // ── float compare ──────────────────────────────────────────────────
        case OP_FEQ: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(to_f(a)==to_f(b))); } break;
        case OP_FNE: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(to_f(a)!=to_f(b))); } break;
        case OP_FLT: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(to_f(a)< to_f(b))); } break;
        case OP_FLE: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(to_f(a)<=to_f(b))); } break;
        case OP_FGT: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(to_f(a)> to_f(b))); } break;
        case OP_FGE: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(to_f(a)>=to_f(b))); } break;

        // ── bitwise ────────────────────────────────────────────────────────
        case OP_AND: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i& b.i)); } break;
        case OP_OR:  { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i| b.i)); } break;
        case OP_XOR: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i^ b.i)); } break;
        case OP_NOT: { Value a=pop(vm); push(vm,VAL_I(~a.i)); } break;
        case OP_SHL: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I(a.i<<b.i)); } break;
        case OP_SHR: { Value b=pop(vm),a=pop(vm); push(vm,VAL_I((uint64_t)a.i>>b.i)); } break;

        // ── casts ──────────────────────────────────────────────────────────
        case OP_I2F:   { Value a=pop(vm); push(vm,VAL_F((double)a.i));  } break;
        case OP_F2I:   { Value a=pop(vm); push(vm,VAL_I((int64_t)a.f)); } break;
        case OP_WIDEN: break; // values are already i64 internally
        case OP_TRUNC: {
            Value a=pop(vm);
            uint64_t mask = arg==64 ? ~0ULL : ((1ULL<<arg)-1);
            push(vm, VAL_I((int64_t)(a.i & mask)));
        } break;

        // ── control flow ───────────────────────────────────────────────────
        case OP_JMP: IP = arg; break;
        case OP_JZ:  { Value c=pop(vm); if (!to_i(c)) IP=arg; } break;
        case OP_JNZ: { Value c=pop(vm); if ( to_i(c)) IP=arg; } break;

        // ── functions ──────────────────────────────────────────────────────
        case OP_CALL:
        case OP_CALL_PTR: {
            int target_fn;
            if (op == OP_CALL) {
                target_fn = arg;
            } else {
                Value fv = pop(vm);
                assert(fv.kind == VAL_FN);
                target_fn = (int)fv.fn;
            }
            assert(vm->fc < VM_FRAMES_MAX && "call stack overflow");
            // save current frame ip (already incremented past CALL)
            CallFrame* new_frame = &vm->frames[vm->fc++];
            new_frame->chunk      = vm->fns[target_fn];
            new_frame->ip         = 0;
            new_frame->local_base = vm->locals_top;
            vm->locals_top       += new_frame->chunk->local_count;
            new_frame->locals     = vm->locals_store + new_frame->local_base;
            memset(new_frame->locals, 0, new_frame->chunk->local_count * sizeof(Value));
            frame = new_frame;
            // redefine macros for new frame
            #undef CHUNK
            #undef CODE
            #undef IP
            #undef POOL
            #undef LOCAL
            #define CHUNK  frame->chunk
            #define CODE   frame->chunk->code
            #define IP     frame->ip
            #define POOL   frame->chunk->const_pool
            #define LOCAL  frame->locals
        } break;

        case OP_RET: {
            Value ret = pop(vm);
            vm->locals_top -= frame->chunk->local_count;
            vm->fc--;
            if (vm->fc == 0) return ret; // top-level return
            frame = &vm->frames[vm->fc - 1];
            #undef CHUNK
            #undef CODE
            #undef IP
            #undef POOL
            #undef LOCAL
            #define CHUNK  frame->chunk
            #define CODE   frame->chunk->code
            #define IP     frame->ip
            #define POOL   frame->chunk->const_pool
            #define LOCAL  frame->locals
            push(vm, ret);
        } break;

        case OP_RET_VOID: {
            vm->locals_top -= frame->chunk->local_count;
            vm->fc--;
            if (vm->fc == 0) return VAL_VOID_;
            frame = &vm->frames[vm->fc - 1];
            #undef CHUNK
            #undef CODE
            #undef IP
            #undef POOL
            #undef LOCAL
            #define CHUNK  frame->chunk
            #define CODE   frame->chunk->code
            #define IP     frame->ip
            #define POOL   frame->chunk->const_pool
            #define LOCAL  frame->locals
            push(vm, VAL_VOID_);
        } break;

        // ── structs ────────────────────────────────────────────────────────
        case OP_STRUCT_NEW: {
            // operand: (field_count << 16) | type_id
            int field_count = (arg >> 16) & 0xFFFF;
            uint32_t type_id = arg & 0xFFFF;
            uint32_t ptr = heap_alloc(vm, field_count, type_id);
            push(vm, VAL_PTR_(ptr));
        } break;

        case OP_FIELD_GET: {
            Value ptr = pop(vm);
            assert(ptr.kind == VAL_PTR);
            HeapObj* obj = &vm->heap[ptr.ptr];
            assert(arg < obj->field_count);
            push(vm, obj->fields[arg]);
        } break;

        case OP_FIELD_SET: {
            Value val = pop(vm);
            Value ptr = pop(vm);
            assert(ptr.kind == VAL_PTR);
            HeapObj* obj = &vm->heap[ptr.ptr];
            assert(arg < obj->field_count);
            obj->fields[arg] = val;
        } break;

        // ── pointers ───────────────────────────────────────────────────────
        case OP_DEREF: {
            Value ptr = pop(vm);
            if (ptr.kind == VAL_PTR) {
                if (ptr.ptr & 0x80000000u) {
                    // local ptr
                    uint32_t idx = ptr.ptr & ~0x80000000u;
                    push(vm, vm->locals_store[idx]);
                } else {
                    // heap ptr — dereference as the struct itself
                    // (pushing the pointer back since structs are ref types)
                    push(vm, ptr);
                }
            } else {
                VM_ERR(vm, "deref on non-pointer");
            }
        } break;

        case OP_STORE_PTR: {
            Value val = pop(vm);
            Value ptr = pop(vm);
            if (ptr.kind == VAL_PTR) {
                if (ptr.ptr & 0x80000000u) {
                    uint32_t idx = ptr.ptr & ~0x80000000u;
                    vm->locals_store[idx] = val;
                } else {
                    // for struct pointers: copy fields if both are structs
                    if (val.kind == VAL_PTR && !(val.ptr & 0x80000000u)) {
                        HeapObj* dst = &vm->heap[ptr.ptr];
                        HeapObj* src = &vm->heap[val.ptr];
                        assert(dst->field_count == src->field_count);
                        memcpy(dst->fields, src->fields, src->field_count * sizeof(Value));
                    } else {
                        VM_ERR(vm, "store_ptr type mismatch");
                    }
                }
            } else {
                VM_ERR(vm, "store_ptr on non-pointer");
            }
        } break;

        // ── stack ──────────────────────────────────────────────────────────
        case OP_POP: pop(vm); break;
        case OP_DUP: push(vm, peek_vm(vm, 0)); break;

        case OP_HALT: goto done;

        default:
            VM_ERR(vm, "unknown opcode %d", op);
            break;
        }
    }
done:
    if (vm->sp > 0) return pop(vm);
    return VAL_VOID_;

    #undef CHUNK
    #undef CODE
    #undef IP
    #undef POOL
    #undef LOCAL
}

// ─── Disassembler ────────────────────────────────────────────────────────────

static const char* op_name(OpCode op) {
    switch(op) {
    case OP_CONST_I:     return "CONST_I";
    case OP_CONST_F:     return "CONST_F";
    case OP_CONST_VOID:  return "CONST_VOID";
    case OP_LOAD:        return "LOAD";
    case OP_STORE:       return "STORE";
    case OP_LOAD_ADDR:   return "LOAD_ADDR";
    case OP_LOAD_GLOBAL: return "LOAD_GLOBAL";
    case OP_STORE_GLOBAL:return "STORE_GLOBAL";
    case OP_IADD:        return "IADD";
    case OP_ISUB:        return "ISUB";
    case OP_IMUL:        return "IMUL";
    case OP_IDIV:        return "IDIV";
    case OP_IMOD:        return "IMOD";
    case OP_INEG:        return "INEG";
    case OP_FADD:        return "FADD";
    case OP_FSUB:        return "FSUB";
    case OP_FMUL:        return "FMUL";
    case OP_FDIV:        return "FDIV";
    case OP_FNEG:        return "FNEG";
    case OP_IEQ:         return "IEQ";
    case OP_INE:         return "INE";
    case OP_ILT:         return "ILT";
    case OP_ILE:         return "ILE";
    case OP_IGT:         return "IGT";
    case OP_IGE:         return "IGE";
    case OP_FEQ:         return "FEQ";
    case OP_FNE:         return "FNE";
    case OP_FLT:         return "FLT";
    case OP_FLE:         return "FLE";
    case OP_FGT:         return "FGT";
    case OP_FGE:         return "FGE";
    case OP_AND:         return "AND";
    case OP_OR:          return "OR";
    case OP_XOR:         return "XOR";
    case OP_NOT:         return "NOT";
    case OP_SHL:         return "SHL";
    case OP_SHR:         return "SHR";
    case OP_I2F:         return "I2F";
    case OP_F2I:         return "F2I";
    case OP_WIDEN:       return "WIDEN";
    case OP_TRUNC:       return "TRUNC";
    case OP_JMP:         return "JMP";
    case OP_JZ:          return "JZ";
    case OP_JNZ:         return "JNZ";
    case OP_CALL:        return "CALL";
    case OP_CALL_PTR:    return "CALL_PTR";
    case OP_RET:         return "RET";
    case OP_RET_VOID:    return "RET_VOID";
    case OP_STRUCT_NEW:  return "STRUCT_NEW";
    case OP_FIELD_GET:   return "FIELD_GET";
    case OP_FIELD_SET:   return "FIELD_SET";
    case OP_DEREF:       return "DEREF";
    case OP_STORE_PTR:   return "STORE_PTR";
    case OP_POP:         return "POP";
    case OP_DUP:         return "DUP";
    case OP_HALT:        return "HALT";
    default:             return "???";
    }
}

void vm_disasm(VM* vm, int fn_index) {
    Chunk* c = vm->fns[fn_index];
    printf("=== fn[%d] 'fn'  locals=%d ===\n", fn_index, /*c->name,*/ c->local_count);
    for (int i = 0; i < c->code_len; i++) {
        Instr in = c->code[i];
        printf("  %4d  %-16s", i, op_name(in.op));
        switch (in.op) {
        case OP_CONST_I: {
            Value v = c->const_pool[in.operand];
            printf(" [%d] = %lld", in.operand, (long long)v.i);
        } break;
        case OP_CONST_F: {
            Value v = c->const_pool[in.operand];
            printf(" [%d] = %g", in.operand, v.f);
        } break;
        case OP_STRUCT_NEW: {
            printf(" fields=%d type=%d", (in.operand>>16)&0xFFFF, in.operand&0xFFFF);
        } break;
        default:
            if (in.operand) printf(" %d", in.operand);
        }
        printf("\n");
    }
}
int vm_get_fn_index(VM* vm, const char* name) {
    for (int i = 0; i < vm->fn_count; i++) {
        // printf("%s...\n", vm->fns[i]->name);
        if (vm->fns[i]->name)
            if (strcmp(vm->fns[i]->name, name) == 0)
                return i;
    }
    return -1;
}
