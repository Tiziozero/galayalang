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
struct ProgramState {
    GalaCommand command;
};
int gala_parse_args(char* paths[10], int argc, char** argv) {
    memset(paths, 0, 10*sizeof(char*));
    if (argc < 2) {
        err("More than one arg expected.");
        return 0;
    } else if (argc > 10) {
        err("No more than ten args expected for now.");
        return 0;
    }
    int i = 1;
    while (i < argc && i < 0) {
        paths[i] = argv[i];
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
int main(int argc, char** argv) {
    dbg("Log level %d", LOG_LEVEL);
    char* paths[10];
    if (!gala_parse_args(paths, argc, argv)) {
        err("Failed to parse args.");
        return 0;
    }
    int status = 0;
    char* path;
    if (argc < 2) {
        // err( "Expected arguments.\nUsage: <program> <file>\n");
        // return 1;
        path = "main.gala";
    } else {
        path = argv[1];
    }
    // read file
    FILE* f = fopen(path, "rb");
    if (!f) {
        err( "Couldn't open file %s", path);
        return 1;
    }
    dbg("opened file");
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);
    dbg("got end");
    char buf[1024*1024];
    fread(buf, 1, sizeof(buf), f);
    dbg("read file");
    fclose(f); // free file
    dbg("closed file");
    buf[length] = '\0';

    Lexer* l = lexer(buf, length);
    if (!l) {
        err( "Failed to lex (?) file.");
        free(f);
        return 1;
    }
    // create lines to print
    l->code = buf;
    char* code_copy = malloc(length*sizeof(char));
    memset(code_copy, 0, length*sizeof(char));
    memcpy(code_copy, buf, length*sizeof(char));
    size_t out = 0;

    char** lines = split_lines(code_copy, &out);
    if (!lines) panic("Failed to split code into lines");
    l->lines = lines;
    l->lines_count = out;


    ParserCtx* pctx = parse(l);
    if (pctx == NULL) {
            err("Failed to parse Tokens.");
            return 1;
    }

    if (!code_gen(pctx)) {
        err("Couldn't generate code.");
        status = 1;
    } else {
        info("Code gen successful");
    }

    info("End parser");
    if (!pctx_destry(pctx)) {
        err("Failed to free parser context");
    }
    info("freeing lexer");
    free(l->tokens);
    free(l);
    dbg("Finished.");
    return status;
}

