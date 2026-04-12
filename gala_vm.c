#include "gala_vm.h"
#include "logger.h"
#include "utils.h"
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
 
void vm_disasm(VM* vm, int fn_index) {
    for (int i = 0; i < 20; i++) {
    }
}
#define VM_MEM (1 << 20)
#define VM_STACK_START (VM_MEM - 4096) // 4KB stack at top


VM vm_new(int stack_kb) {
    VM vm;
    memset(&vm, 0, sizeof(VM));
    vm.sp = VM_MEM - (stack_kb * 1024);
    return vm;
}

// read 4 bytes from addr, no pc advance
uint32_t fetch4(VM* vm, uint32_t addr) {
    uint32_t val;
    memcpy(&val, &vm->mem[addr], 4);
    return val;
}

// read next instruction word and advance pc
uint32_t fetch_next(VM* vm) {
    uint32_t val = fetch4(vm, vm->pc);
    vm->pc += 4;
    return val;
}

int pop4(VM* vm) {
    vm->sp -= 4;
    int a;
    memcpy(&a, &vm->mem[vm->sp], 4);
    return a;
}

void push4(VM* vm, int val) {
    memcpy(&vm->mem[vm->sp], &val, 4);
    vm->sp += 4;
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
    long size = rb - buf;
    VM _vm = vm_new(1024);
    VM* vm = &_vm;
    // copy to memory
    memcpy(vm->mem, buf, size);
    dbg("Size of opcode %d", sizeof(OpCode));


    assert(size%4 == 0);
    for (int i = 0; i < size; i+=4) {
        printf(" === %5d === %.2x %.2x %.2x %.2x\n",
                i, buf[i], buf[i+1], buf[i+2], buf[i+3]);
    }


    while (!vm->had_error) {
        dbg("pc %d", vm->pc);
        OpCode op = (OpCode)fetch_next(vm);
        dbg("Op %d pc %d", op, vm->pc);
        switch (op) {
            case OP_PUSH_CONST_I: {
                int32_t val = (int32_t)fetch_next(vm); // operand is next word
                push4(vm, val);
            } break;
            case OP_PUSH_CONST_F: {
                // same bits, just reinterpreted by float ops later
                int32_t val = (int32_t)fetch_next(vm);
                push4(vm, val);
            } break;

            case OP_LOAD_4: {
                uint32_t ptr = (uint32_t)pop4(vm);
                push4(vm, (int32_t)fetch4(vm, ptr));
            } break;
            case OP_STORE_4: {
                uint32_t ptr = (uint32_t)pop4(vm);
                int32_t val = pop4(vm);
                memcpy(&vm->mem[ptr], &val, 4);
            } break;

            case OP_LOAD_4_OFF: {
                uint32_t ptr = (uint32_t)pop4(vm);
                int32_t off = (int32_t)fetch_next(vm); // offset is immediate
                push4(vm, (int32_t)fetch4(vm, ptr + off));
            } break;
            case OP_STORE_4_OFF: {
                uint32_t ptr = (uint32_t)pop4(vm);
                int32_t val = pop4(vm);
                int32_t off = (int32_t)fetch_next(vm);
                memcpy(&vm->mem[ptr + off], &val, 4);
            } break;

            case OP_ADDI: { int b = pop4(vm); int a = pop4(vm); push4(vm, a + b); } break;
            case OP_SUBI: { int b = pop4(vm); int a = pop4(vm); push4(vm, a - b); } break;
            case OP_MLTI: { int b = pop4(vm); int a = pop4(vm); push4(vm, a * b); } break;
            case OP_DIVI: {
                int b = pop4(vm); int a = pop4(vm);
                if (b == 0) { snprintf(vm->error, 256, "division by zero at pc=%d", vm->pc); vm->had_error = 1; break; }
                push4(vm, a / b);
            } break;

            case OP_ADDF: { float b, a; int bi = pop4(vm), ai = pop4(vm); memcpy(&b, &bi, 4); memcpy(&a, &ai, 4); float r = a+b; int ri; memcpy(&ri, &r, 4); push4(vm, ri); } break;
            case OP_SUBF: { float b, a; int bi = pop4(vm), ai = pop4(vm); memcpy(&b, &bi, 4); memcpy(&a, &ai, 4); float r = a-b; int ri; memcpy(&ri, &r, 4); push4(vm, ri); } break;
            case OP_MLTF: { float b, a; int bi = pop4(vm), ai = pop4(vm); memcpy(&b, &bi, 4); memcpy(&a, &ai, 4); float r = a*b; int ri; memcpy(&ri, &r, 4); push4(vm, ri); } break;
            case OP_DIVF: { float b, a; int bi = pop4(vm), ai = pop4(vm); memcpy(&b, &bi, 4); memcpy(&a, &ai, 4); float r = a/b; int ri; memcpy(&ri, &r, 4); push4(vm, ri); } break;

            case OP_CMPI: { int b = pop4(vm); int a = pop4(vm); push4(vm, a == b ? 1 : 0); } break;
            case OP_CMPF: {
                int bi = pop4(vm), ai = pop4(vm); float a, b;
                memcpy(&a, &ai, 4); memcpy(&b, &bi, 4);
                push4(vm, a == b ? 1 : 0);
            } break;

            case OP_JMP: { uint32_t addr = (uint32_t)fetch_next(vm); vm->pc = addr; } break;
            case OP_JPT: { uint32_t addr = (uint32_t)fetch_next(vm); int cond = pop4(vm); if (cond) vm->pc = addr; } break;
            case OP_JP0: { uint32_t addr = (uint32_t)fetch_next(vm); int cond = pop4(vm); if (!cond) vm->pc = addr; } break;

            case OP_HLT: { vm->had_error = 1; strcpy(vm->error, "returned"); } break; // clean exit, same flag for now

            default:
                snprintf(vm->error, 256, "unknown opcode %d at pc=%d", (int)op, vm->pc - 4);
                vm->had_error = 1;
                break;
        }
    }
vm_done:
    if (vm->had_error) fprintf(stderr, "vm error: %s\n", vm->error);
    vm_disasm(vm, 0);

    return 0;
}
