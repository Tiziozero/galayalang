#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
#include "logger.h"
#include "lexer.h"
#include "parser.h"
#include "code_gen.h"

int main(int argc, char** argv) {
    int status = 0;
    if (argc < 2) {
        err( "Expected arguments.\nUsage: <program> <file>\n");
        return 1;
    }
    // read file
    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        err( "Couldn't open file %s", argv[1]);
        return 1;
    }
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);
    char buf[1024*1024];
    fread(buf, 1, sizeof(buf), f);
    buf[length] = '\0';

    Lexer* l = lexer(buf, length);
    if (!l) {
        err( "Failed to lexe (?) file.");
        free(f);
        return 1;
    }

    Arena a = arena_new(1024, sizeof(Node));
    AST* ast = parse(l, &a);
    if (ast == NULL) {
            err( "Fauled to parse Tokens.");
            return 1;
    }

    if (!code_gen(ast)) {
        err("Couldn't generate code.");
        status = 1;
    }

    for (size_t i = 0; i < a.pages_count; i++) {
        free(a.pages[i]);
    }
    free(a.pages);
    free(ast->nodes); // free nodes
    free(ast); // then AST
    free(l->tokens);
    free(l);
    fclose(f);
    return status;
}

