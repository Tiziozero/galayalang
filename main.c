#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "logger.h"
#include "lexer.h"
#include "parser.h"
#include "code_gen.h"
#include <stdlib.h>
#include <stddef.h>

typedef enum { CMD_BUILD, CMD_CHECK, CMD_RUN } GalaCommand;

typedef struct {
    int index;
    char* arg;
} Arg;

Arg available_args[] = {};
typedef struct ProgramState ProgramState;
struct ProgramState {
    GalaCommand command;
};
int gala_parse_args(char* paths[10], int argc, char** argv) {
    memset(paths, 0, 10*sizeof(char*));
    if (argc < 2) {
        err("More than one arg expected.");
        return 0;
    } else if (argc > 11) { // program + 10 (max) files
        err("No more than ten args expected for now.");
        return 0;
    }
    int i = 0;
    info("%zu args", argc);
    while (i < argc && i < 10) {
        info("arg %s...", argv[i]);
        paths[i] = argv[i+1]; // since it starts at 1
        i++;
    }
    return 1;
}
char **split_lines(char *src, size_t *out_count) {
    size_t cap = 16;
    size_t count = 0;
    char **lines = malloc(cap * sizeof(char *));
    if (!lines) return NULL;

    char *p = src;
    lines[count++] = p;

    while (*p) {
        if (*p == '\r') {
            *p = '\0';
            if (p[1] == '\n')
                p++;            // swallow \n in \r\n
            p++;
            if (*p) {
                if (count == cap) {
                    cap *= 2;
                    lines = realloc(lines, cap * sizeof(char *));
                    if (!lines) return NULL;
                }
                lines[count++] = p;
            }
            continue;
        }

        if (*p == '\n') {
            *p = '\0';
            p++;
            if (*p) {
                if (count == cap) {
                    cap *= 2;
                    lines = realloc(lines, cap * sizeof(char *));
                    if (!lines) return NULL;
                }
                lines[count++] = p;
            }
            continue;
        }

        p++;
    }

    *out_count = count;
    return lines;
}
ParserCtx*      handle_new_file(char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) {
        err( "Couldn't open file %s", path);
        return 0;
    }
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);
    char* buf = malloc(length *sizeof(char) + 1); // + 1 for \0
    if (!buf) {
        err("Failed to allocate buffer.");
        return 0;
    }
    fread(buf, 1, length*sizeof(char), f);
    buf[length] = '\0'; // endline
    fclose(f); // free file

    Lexer* l = lexer(buf, length);
    if (!l) {
        err( "Failed to lex (?) file.");
        return 0;
    }
    l->code = buf;
    // create lines to print
    char* code_copy = malloc(length*sizeof(char));
    memset(code_copy, 0, length*sizeof(char));
    memcpy(code_copy, buf, length*sizeof(char));
    size_t out = 0;
    char** lines = split_lines(code_copy, &out);
    if (!lines) panic("Failed to split code into lines");
    l->lines_buf = code_copy;
    l->lines = lines;
    l->lines_count = out;


    ParserCtx* pctx = parse(l);
    if (pctx == NULL) {
        err("Failed to parse Tokens.");
        return 0;
    }
    return pctx;
}
int main(int argc, char** argv) {
    dbg("Log level %d", LOG_LEVEL);
    char* paths[10];
    if (!gala_parse_args(paths, argc, argv)) {
        err("Failed to parse args.");
        return 0;
    }
    for (size_t i = 0; paths[i] != 0 && i < 10; i++) {
        char* path = paths[i];
        dbg("file %s...", path);

        ParserCtx* pctx = handle_new_file(path);
        // codegen

        if (!pctx_destry(pctx)) {
            err("Failed to free parser context");
            return 1;
        }
        info("freed file %zu\n");
    }

    dbg("Finished.");
    return 0;
}

