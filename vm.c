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
