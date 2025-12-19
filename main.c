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
    /* for (int i = 0; i < l->tokens_count; i++) {
        Token t = l->tokens[i];
        info("Token: %s\t\t",get_token_type(t.type));

        if (t.type == TokenIdent) {
            fwrite(t.ident.name, 1, t.ident.length, stdout);
        }
        if (t.type == TokenKeyword) {
            Name name = get_keyword_name(t.kw);
            fwrite(name.name, 1, name.length, stdout);
        }
        if (t.type == TokenNumber) {
            fwrite(t.number.name, 1, t.number.length, stdout);
        }
        info("\n");
    } */

    Arena a = arena_new(1024);
    AST* ast = parse(l, &a);
    if (ast == NULL) {
            err( "Fauled to parse Tokens.");
            return 1;
    }

    if (!code_gen(ast)) {
        err("Couldn't generate code.");
        status = 1;
    }

    free(a.memory); // free arena memory
    free(ast->nodes); // free nodes
    free(ast); // then AST
    free(l->tokens);
    free(l);
    fclose(f);
    return status;
}

