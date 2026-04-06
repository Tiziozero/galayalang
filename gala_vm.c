#include "gala_vm.h"
#include "logger.h"
#include "utils.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
static const char* op_name(OpCode op);
int exec(VM* vm) {
    #define fetch() vm->mem[vm->pc++]
    #define peek() vm->mem[vm->pc]
    #define mem_get(addr) vm->mem[addr]
    #define mem_set(addr, x) vm->mem[addr] = x
    #define stack_push(x) vm->mem[vm->sp--] = x
    #define stack_pop() vm->mem[vm->sp++]
    Value v = fetch();
    if (v.kind !=VAL_OP) {
        panic("pc value is not an op.");
        return 0;
    }
    Instr i = v.op;
    switch (i.op) {
        case OP_CONST_I:
        case OP_CONST_F:
            {
                Value v = fetch();
                stack_push(v);
            } break;
        case OP_CONST_VOID:
            {
                Value v = {0}; // void
                stack_push(v);
            } break;
        case OP_LOAD:
            {
                Value addr = fetch();
                stack_push(mem_get(addr.ptr)); // use ptr. ofc
            } break;
        case OP_STORE: // addr, value
            {
                Value addr = fetch(); 
                Value v = fetch(); 
                mem_set(addr.ptr, v);
            } break;
        case OP_LOAD_ADDR: // ptr/reference
            {
                TODO("Figure out smth here i really don't know.");
            } break;
        case OP_IADD:
            {
                Value v1 = stack_pop(), v2 = stack_pop(),
                      v3 = VAL_I(v1.i + v2.i);
                stack_push(v3);
            } break;
        default: panic("unhandled opcode %s", op_name(i.op));
    }

    #undef peek
    #undef fetch
    return 0;
}
int vm_run(VM* vm, int start) {
    vm->pc = start;
    while (!vm->had_error) {
        exec(vm);
    }

    return 0;
}
VM    vm_new(int global_count) {
    VM vm;
    memset(&vm, 0, sizeof(VM));
    vm.sp = VM_MEM-1;
    return vm;
}

static const char* op_name(OpCode op) {
    switch(op) {
    case OP_CONST_I:     return "CONST_I";
    case OP_CONST_F:     return "CONST_F";
    case OP_CONST_VOID:  return "CONST_VOID";
    case OP_LOAD:        return "LOAD";
    case OP_STORE:       return "STORE";
    case OP_LOAD_ADDR:   return "LOAD_ADDR";
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
    for (int i = 0; i < 20; i++) {
        Value v = vm->mem[i];
        printf(" === [%7d:%d] === ", i,v.kind);
        switch (v.kind) {
            case VAL_I64:
                printf(" %7zu", v.i);
                break;
            case VAL_F64:
                printf(" %7f", v.f);
                break;
            case VAL_PTR:
                printf(" %7u", v.ptr);
                break;
            case VAL_OP:
                printf(" %s", op_name(v.op.op));
                break;
            default:
                panic("Handle %d.", v.kind);
        }
        printf("\n");
    }
}


Value push(VM*vm, Value v) {
    vm->mem[vm->sp++] = v;
    return v;
}
int main(int arc, char** argv) {
    if (arc < 2) {
        err("Needed input file: \"%s <file>\".",  argv[0]);
        return 0;
    }
    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        panic("Failed to open file.");
        return 0;
    }

    char buf[VM_MEM];
    memset(buf, 0, VM_MEM);
    char* rb = buf;
    size_t remaining = VM_MEM;


    size_t n;
    while (remaining > 0 && (n = fread(rb, 1, remaining, f)) > 0) {
        rb += n;
        remaining -= n;
    }
    VM _vm = vm_new(1024);


    VM* vm = &_vm;
    vm->const_pool[0] = VAL_I(2);
    vm->const_pool[1] = VAL_I(3);
    vm->const_pool[2] = VAL_I(4);
    vm->const_pool[3] = VAL_I(5);
    long size = rb - buf;
    #define POP()      (vm->mem[--vm->sp]) // cuz ye ofc
    #define TOP()      (vm->mem[vm->sp - 1])
    #define READ_I32() ({ int32_t _v; memcpy(&_v, &buf[vm->pc], 4); vm->pc += 4; _v; })

    while (!vm->had_error) {
        if (vm->pc >= size) break;
        OpCode op = (OpCode)(uint8_t)buf[vm->pc++];
        dbg("Opcode %s pc %d", op_name(op), vm->sp);

        switch (op) {
            case OP_CONST_I: { int32_t idx = READ_I32(); push(vm,((Value){.kind=VAL_I64, .i=vm->const_pool[idx].i})); break; }
            case OP_CONST_F: { int32_t idx = READ_I32(); push(vm,(Value){.kind=VAL_F64, .f=vm->const_pool[idx].f}); break; }
            case OP_CONST_VOID: { push(vm,(Value){.kind=VAL_VOID}); break; }

            case OP_LOAD:      { int32_t s=READ_I32(); push(vm,vm->mem[s]); break; }
            case OP_STORE:     { int32_t s=READ_I32(); vm->mem[s]=POP(); break; }
            case OP_LOAD_ADDR: { int32_t s=READ_I32(); push(vm,(Value){.kind=VAL_PTR,.ptr=(uint32_t)s}); break; }

            case OP_IADD: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i+b.i}); break; }
            case OP_ISUB: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i-b.i}); break; }
            case OP_IMUL: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i*b.i}); break; }
            case OP_IDIV: { Value b=POP(),a=POP(); if(!b.i){vm->had_error=1;snprintf(vm->error,256,"div by zero");break;} push(vm,(Value){.kind=VAL_I64,.i=a.i/b.i}); break; }
            case OP_IMOD: { Value b=POP(),a=POP(); if(!b.i){vm->had_error=1;snprintf(vm->error,256,"mod by zero");break;} push(vm,(Value){.kind=VAL_I64,.i=a.i%b.i}); break; }
            case OP_INEG: { Value a=POP(); push(vm,(Value){.kind=VAL_I64,.i=-a.i}); break; }

            case OP_FADD: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_F64,.f=a.f+b.f}); break; }
            case OP_FSUB: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_F64,.f=a.f-b.f}); break; }
            case OP_FMUL: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_F64,.f=a.f*b.f}); break; }
            case OP_FDIV: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_F64,.f=a.f/b.f}); break; }
            case OP_FNEG: { Value a=POP(); push(vm,(Value){.kind=VAL_F64,.f=-a.f}); break; }

            case OP_IEQ: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i==b.i}); break; }
            case OP_INE: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i!=b.i}); break; }
            case OP_ILT: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i< b.i}); break; }
            case OP_ILE: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i<=b.i}); break; }
            case OP_IGT: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i> b.i}); break; }
            case OP_IGE: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i>=b.i}); break; }
            case OP_FEQ: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.f==b.f}); break; }
            case OP_FNE: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.f!=b.f}); break; }
            case OP_FLT: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.f< b.f}); break; }
            case OP_FLE: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.f<=b.f}); break; }
            case OP_FGT: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.f> b.f}); break; }
            case OP_FGE: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.f>=b.f}); break; }

            case OP_AND: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i& b.i}); break; }
            case OP_OR:  { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i| b.i}); break; }
            case OP_XOR: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i^ b.i}); break; }
            case OP_NOT: { Value a=POP(); push(vm,(Value){.kind=VAL_I64,.i=~a.i}); break; }
            case OP_SHL: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=a.i<<b.i}); break; }
            case OP_SHR: { Value b=POP(),a=POP(); push(vm,(Value){.kind=VAL_I64,.i=(int64_t)((uint64_t)a.i>>b.i)}); break; }

            case OP_I2F: { Value a=POP(); push(vm,(Value){.kind=VAL_F64,.f=(double)a.i}); break; }
            case OP_F2I: { Value a=POP(); push(vm,(Value){.kind=VAL_I64,.i=(int64_t)a.f}); break; }
            case OP_WIDEN: { /* already i64 */ break; }
            case OP_TRUNC: {
                               int32_t bits=READ_I32(); Value a=POP();
                               int64_t mask = (bits>=64) ? ~0LL : (1LL<<bits)-1;
                               push(vm,(Value){.kind=VAL_I64,.i=a.i&mask}); break;
                           }

            case OP_JMP: { vm->pc=READ_I32(); break; }
            case OP_JZ:  { int32_t t=READ_I32(); if(!POP().i) vm->pc=t; break; }
            case OP_JNZ: { int32_t t=READ_I32(); if( POP().i) vm->pc=t; break; }

            case OP_POP:  { POP(); break; }
            case OP_DUP:  { Value a=TOP(); push(vm,a); break; }
            case OP_HALT: goto vm_done;

                          // stubs — implement when you add call frames / heap
            case OP_CALL:
            case OP_CALL_PTR:
            case OP_RET:
            case OP_RET_VOID:
            case OP_STRUCT_NEW:
            case OP_FIELD_GET:
            case OP_FIELD_SET:
            case OP_DEREF:
            case OP_STORE_PTR:
                          snprintf(vm->error, 256, "unimplemented opcode %d at pc=%d", (int)op, vm->pc-1);
                          vm->had_error = 1;
                          break;

            default:
                          snprintf(vm->error, 256, "unknown opcode %d at pc=%d", (int)op, vm->pc-1);
                          vm->had_error = 1;
                          break;
        }
    }
vm_done:
    if (vm->had_error) fprintf(stderr, "vm error: %s\n", vm->error);
    vm_disasm(vm, 0);
    printf("at sp %zu (sp %d)\n", vm->mem[vm->sp-1].i, vm->sp-1);

    #undef push
    #undef POP
    #undef TOP
    #undef READ_I32
    return 0;
}
