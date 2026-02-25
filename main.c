#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "code_gen.h"
#include "logger.h"
#include "lexer.h"
#include "parser.h"
#include "utils.h"
// #include "code_gen.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#define CODE_LEN 32
#if 1
    #ifdef LOG_LEVEL
        #undef LOG_LEVEL
        #define LOG_LEVEL 5
    #endif
#endif

#include <stdlib.h>
#include <time.h>

char *random_string(size_t n) {
    const char charset[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    char *str = malloc(n + 1);
    if (!str) return NULL;

    for (size_t i = 0; i < n; i++) {
        int key = rand() % (sizeof(charset) - 1);
        str[i] = charset[key];
    }

    str[n] = '\0';
    return str;
}
typedef enum { CMD_BUILD, CMD_CHECK, CMD_RUN } GalaCommand;

typedef struct {
    int index;
    char* arg;
} Arg;

Arg available_args[] = {};
struct ProgramState {
    GalaCommand command;
    ParserCtx** files;
    size_t files_count;
    size_t files_cap;
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
                    lines =
                        realloc(lines, cap * sizeof(char *));
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
char** paths = 0;
size_t plen = 0;
size_t pcap = 0;
ParserCtx* handle_new_file(ProgramState* ps, char* path) {
    if (!paths) {
        pcap = 10;
        paths = malloc(pcap*sizeof(char*));
    }
    info("%zu", ps);
    for (size_t i = 0; i < ps->files_count; i++) {
        if (strcmp(ps->files[i]->path, path) == 0) {
            info("pctx %s exists.", path);
            return ps->files[i];
        }
    }
    // add to paths. if it doesn't exists in pctx but exists
    // here it is being evaluated thus circular dependance
    for (size_t i = 0; i < plen; i++) {
        if (strcmp(path, paths[i]) == 0) {
            err("path %s already exists");
            return NULL;
        }
    }
    if (plen >= pcap) {
        pcap*= 2;
        paths = realloc(paths,pcap*sizeof(char*));
    }
    paths[plen++] = path;
    char* pctx_code = random_string(CODE_LEN);
    Name name = get_name_from_path(path);
    if (name.name == 0 || name.length == 0) {
        err("failed to get name from path \"%s\".");
        return NULL;
    }

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



    ParserCtx* pctx = pctx_new(l->code, l->tokens, l->tokens_count, l, path, name);
    pctx->ps = ps;
    if (!pctx) {
        err("Failed to create parser context.");
        return 0;
    }

    if (!parse(pctx)) {
        err("Failed to parse Tokens.");
        return 0;
    }
    if (ps->files_count >= ps->files_cap) {
        size_t new_cap = ps->files_cap ? ps->files_cap * 2 : 4; // start with 4 if 0
        ParserCtx** new_files = realloc(ps->files, new_cap * sizeof(*ps->files));
        if (!new_files) {
            // handle allocation failure
            // e.g., exit, return error, or assert
            perror("realloc failed");
            exit(1);
        }
        ps->files = new_files;
        ps->files_cap = new_cap;
    }

    // now safe to append
    ps->files[ps->files_count++] = pctx;

    plen--; // pop, no longer being evaluated.
    return pctx;
}
int main(int argc, char** argv) {
    info("Log level %d arc %d", LOG_LEVEL, argc);
    char* paths[10];
    if (!gala_parse_args(paths, argc, argv)) {
        err("Failed to parse args.");
        return 0;
    }
    ProgramState ps;
    ps.files = malloc(10*sizeof(ParserCtx*));
    ps.files_cap = 10;
    ps.files_count = 0;
    size_t errs = 0;
    for (size_t i = 0; paths[i] != 0 && i < 10; i++) {
        char* path = paths[i];
        dbg("file %s...", path);

        ParserCtx* pctx = handle_new_file(&ps, path);
        if (!pctx) {
            err("Failed to parse file \"%s\".", path);
            errs++;
            continue;
        }
        // ps.files[ps.files_count++] = pctx;
        // codegen
    }
    // codegen
    for (size_t i = 0; i < ps.files_count; i++) {
        printf("generating pctx %zu...\n", i);
        fflush(stdout);
        ParserCtx* pctx = ps.files[i];
        if (!code_gen(pctx)) {
            err("Failed to generate file for %.*s",
                    (int)pctx->module_name.length,
                    pctx->module_name.name);
            return 1;
        }
        info("generated file %zu\n", i);
    }
    // free
    for (size_t i = 0; i < ps.files_count; i++) {
        printf("freeing pctx %zu...\n", i);
        fflush(stdout);
        ParserCtx* pctx = ps.files[i];
        if (!pctx_destry(pctx)) {
            err("Failed to free parser context");
            return 1;
        }
        info("freed file %zu\n", i);
    }
    free(ps.files);

    printf("Finished.\n");
    return errs != 0;
}

