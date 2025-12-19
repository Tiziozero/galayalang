#include "code_gen.h"
#include <stdio.h>
int code_gen(AST* ast) {
    char* path = "gala.out";
    FILE* f = fopen(path, "wb");
    if (!f) {
        err("Failed to open output file.");
        return 0;
    }

    fprintf(f, "Hello, World");

    fclose(f);
    return 1;
}

