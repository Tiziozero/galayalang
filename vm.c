#include <gala.h>
#include <stdint.h>
#include <stdio.h>
#define GALA_INCLUDE
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
    for(size_t i = 0; i < len; i++) {
        printf("%.02x ", buf[i]);
    }
    try(buf[len]==0);
    println("");

    char prog_1[] = {
        make_instruction(VM_OP_MOV, VM_OP_REG_IM),
        R1, 8, // 0,
        make_instruction(VM_OP_MOV, VM_OP_REG_IM),
        R2, 7, // 0,
        make_instruction(VM_OP_ADD, VM_OP_REG_REG),
        R1 << 4 | R2,
        make_instruction(VM_OP_ADD, VM_OP_REG_REG),
        R0 << 4 | R1,
        make_instruction(VM_OP_HLT, 0),
    };
    char prog_2[] = { // fix
        make_instruction(VM_OP_MOV, VM_OP_REG_IM),
        R1, 8, // 0, // 1 // 2
        make_instruction(VM_OP_PUSH, 0),
        R1, // 4
        make_instruction(VM_OP_MOV, VM_OP_REG_IM),
        R1, 5, // 7
        make_instruction(VM_OP_PUSH, 0),
        R1, // 9
        make_instruction(VM_OP_PUSH, 15), // 10
        make_instruction(VM_OP_JMP, 0), // 11
        19, 0, // 12 13 -> jump to 20
        make_instruction(VM_OP_HLT, 0), // 14
        make_instruction(VM_OP_NOP, 0), // 15
        make_instruction(VM_OP_NOP, 0), // 16
        make_instruction(VM_OP_NOP, 0), // 17
        make_instruction(VM_OP_NOP, 0), // 18
        make_instruction(VM_OP_POP, 0), R1, // 19, 20
        make_instruction(VM_OP_POP, 0), R2, // 21, 22
        make_instruction(VM_OP_ADD, VM_OP_REG_REG), R1 << 4 | R2, // 23,24,25
        make_instruction(VM_OP_ADD, VM_OP_REG_REG),
        R0 << 4 | R1,
        make_instruction(VM_OP_JMP, 0), // 12
        14,
    };



    int i = run(prog_1, sizeof(prog_1)/sizeof(prog_1[0]));
    Info("prog_1 Returned %d\n", i);
    i = run(prog_2, sizeof(prog_2)/sizeof(prog_2[0]));
    Info("prog_2 Returned %d\n", i);
    free(buf);
    return 0;
}
