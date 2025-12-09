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
        R1, 8,
        make_instruction(VM_OP_MOV, VM_OP_REG_IM),
        R2, 7,
        make_instruction(VM_OP_ADD, VM_OP_REG_REG),
        R1 << 4 | R2,
        make_instruction(VM_OP_HLT, 0),

    };
    printf("%s", buf);
    int i = run(newbuf, sizeof(newbuf)/sizeof(newbuf[0]));
    free(buf);
    return i;
}
int run(char* buf, usize len) {
    cpu8_t* cpu = cpu8_create();
    uint8_t c;
    uint8_t on = 1;
    usize i = 0;
    while (i < len && on) {
        c = buf[i++];
        switch (c>>3) {
            case VM_OP_NOP: Info("VM_OP_NOP\n"); break;
            case VM_OP_HLT: Info("VM_OP_HLT\n");on = 0; break;
            case VM_OP_MOV: {
                Info("mov ");
                uint8_t type = c & 0b0000111;
                switch (type) {
                    case VM_OP_REG_IM: {
                        uint8_t reg = buf[i++];
                        uint8_t value = buf[i++];
                        printf("reg %u value %u\n", reg, value);
                        cpu->reg[reg] = value;
                        break;
                    }
                    case VM_OP_REG_REG: {
                        uint8_t regs = buf[i++];
                        uint8_t r1 = regs >> 4;
                        uint8_t r2 = regs | 0xf;
                        cpu->reg[r1] = cpu->reg[r2];
                        break;
                    }
                    default: FAILED("Invalid VM_OP_TYPE: %u", type);
                };
                break;
            }
            case VM_OP_ADD: {
                Info("add ");
                uint8_t type = c & 0b0000111;
                switch (type) {
                    case VM_OP_REG_IM: {
                        uint8_t reg = buf[i++];
                        uint8_t value = buf[i++];
                        cpu->reg[reg] += value;
                        break;
                    }
                    case VM_OP_REG_REG: {
                        uint8_t regs = buf[i++];
                        uint8_t r1 = regs >> 4;
                        uint8_t r2 = regs & 0xf;
                        printf("r1 %u r2 %u\n", r1, r2);
                        cpu->reg[R0] = cpu->reg[r1] + cpu->reg[r2];
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
