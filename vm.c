#include <gala.h>
#include <stdint.h>
#include <stdio.h>
#include "vm.h"
int run(char* buf, usize len);
int main(int argc, char** argv) {
    try(argc > 1);
    FILE* f = fopen(argv[1], "rb");
    try(f != 0);

    fseek(f, 0, SEEK_END);
    usize len = ftell(f);
    fseek(f, 0, SEEK_SET);

    char* buf = malloc((len)*sizeof(char));
    try(buf != 0);
    fread(buf,1, len, f);
    fclose(f);

    Info("file of size %lu\n", len);
    for(usize i = 0; i < len; i++) {
        printf("%.02x ", buf[i]);
    }
    try(buf[len]==0);
    println("");

    char newbuf[] = {
        make_instruction(VM_OP_MOV, VM_OP_REG_IM),
        R1, 8, 0,
        make_instruction(VM_OP_MOV, VM_OP_REG_IM),
        R2, 7, 0,
        make_instruction(VM_OP_ADD, VM_OP_REG_REG),
        R1 << 4 | R2,
        make_instruction(VM_OP_ADD, VM_OP_REG_REG),
        R0 << 4 | R1,
        make_instruction(VM_OP_HLT, 0),
    };
    printf("%s", buf);
    int i = run(newbuf, sizeof(newbuf)/sizeof(newbuf[0]));
    Info("Returned %d\n", i);
    free(buf);
    return 0;
}
void do_op(uint8_t op, uint8_t* dest, uint8_t src) {
    switch (op) {  // dest, src -> dest = dest + src
        case VM_OP_MOV: *dest = src; break;
        case VM_OP_SUB: *dest -= src; break;
        case VM_OP_ADD: *dest += src; break;
        default: FAILED("Invalid op: %.2x", op);
    
    }
}
int run(char* buf, usize len) {
    cpu8_t* cpu = cpu8_create();
    uint8_t c;
    uint8_t on = 1;
    #define i cpu->pc
    while (i < len && on) {
        c = buf[i++];
        uint8_t op = c>>3;
        switch (op) {
            case VM_OP_NOP: Info("VM_OP_NOP\n"); break;
            case VM_OP_HLT: Info("VM_OP_HLT\n");on = 0; break;
            case VM_OP_MOV:
            case VM_OP_SUB:
            case VM_OP_ADD: { // dest, src -> dest = dest + src
                Info("Got %.2x\n", op);
                uint8_t type = c & 0b0000111;
                switch (type) {
                    case VM_OP_REG_IM: {
                        uint8_t reg = buf[i++];
                        uint8_t value_1 = buf[i++];
                        Info("Immediate to %.2x value %.2x\n", reg, value_1);
                        do_op(op, &cpu->reg[reg], value_1);
                        Info("new dest: %.2x\n", cpu->reg[reg]);
                        break;
                    }
                    case VM_OP_REG_REG: {
                        uint8_t regs = buf[i++];
                        uint8_t r1 = regs >> 4;
                        uint8_t r2 = regs & 0xf;
                        Info("reg-reg to %.2x value %.2x\n", r1, r2);
                        do_op(op, &cpu->reg[r1], cpu->reg[r2]);
                        Info("new dest: %.2x\n", cpu->reg[r1]);
                        break;
                    }
                    case VM_OP_MEM_REG: { // dest, src
                        uint8_t mem_1 = buf[i++];
                        uint8_t mem_2 = buf[i++];
                        uint8_t reg = buf[i++];
                        do_op(op, &cpu->memory[(mem_2 << 8) + mem_1 ], cpu->reg[reg]);
                        break;
                    }
                    default: FAILED("Invalid VM_OP_TYPE: %u", type);
                };
                break;
            }
            default: FAILED("Invalid operation: %u hlt=%u", c, VM_OP_HLT);
        }
    }
    int retval = cpu->reg[R0];
    free(cpu);
    return retval;
}
