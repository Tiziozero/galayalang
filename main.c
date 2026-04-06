/*
 * TODO
 *  change "var" to "obj" in symbol table
 */
#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "logger.h"
#include "lexer.h"
#include "parser.h"
#include "utils.h"
// #include "code_gen.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#if 1
    #ifdef LOG_LEVEL
        #undef LOG_LEVEL
        #define LOG_LEVEL 5
    #endif
#endif

typedef enum { CMD_BUILD, CMD_CHECK, CMD_RUN } GalaCommand;

typedef struct {
    int index;
    char* arg;
} Arg;

Arg available_args[] = {};
struct ProgramState {
    GalaCommand command;
    Parser** files;
    int files_count;
    int files_cap;
};
int gala_parse_args(char* arg_paths[10], int argc, char** argv) {
    memset(arg_paths, 0, 10*sizeof(char*));
    if (argc < 2) {
        err("More than one arg expected.");
        return 0;
    } else if (argc > 11) { // program + 10 (max) files
        err("No more than ten args expected for now.");
        return 0;
    }
    int i = 0;
    dbg("%d args", argc);
    while (i < argc && i < 10) {
        dbg("arg \"%s\"...", argv[i]);
        arg_paths[i] = argv[i+1]; // since it starts at 1
        i++;
    }
    return 1;
}
char **split_lines(char *src, int *out_count) {
    int cap = 16;
    int count = 0;
    char **lines = malloc(cap * sizeof(char *));
    if (!lines) return NULL;

    char *p = src;
    lines[count++] = p;

    while (*p && p) {
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
SymbolTable master_table;
char** open_paths = 0;
int plen = 0;
int pcap = 0;
Parser* handle_new_file(ProgramState* ps, char* path) {
    if (!open_paths) {
        pcap = 10;
        open_paths = malloc(pcap*sizeof(char*));
    }
    for (int i = 0; i < ps->files_count; i++) {
        if (strcmp(ps->files[i]->path, path) == 0) {
            info("pctx %s exists.", path);
            return ps->files[i];
        }
    }
    // add to paths. if it doesn't exists in pctx but exists
    // here it is being evaluated thus circular dependance
    for (int i = 0; i < plen; i++) {
        if (strcmp(path, open_paths[i]) == 0) {
            err("path %s already exists");
            return NULL;
        }
    }
    if (plen >= pcap) {
        pcap*= 2;
        open_paths = realloc(open_paths,pcap*sizeof(char*));
    }
    open_paths[plen++] = path;
    FILE* f = fopen(path, "rb");
    if (!f) {
        panic( "Couldn't open file %s", path);
        return 0;
    }
    fseek(f, 0, SEEK_END);
    int length = ftell(f);
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
    int out = 0;
    char** lines = split_lines(code_copy, &out);
    if (!lines) panic("Failed to split code into lines");
    l->lines_buf = code_copy;
    l->lines = lines;
    l->lines_count = out;



    Parser* pctx = pctx_new(l, path, &master_table);
    if (!pctx) {
        err("Failed to create parser context.");
        return 0;
    }

    dbg("parsing...");
    if (!parse(pctx)) {
        err("Failed to parse Tokens.");
        // free what's to free
        parser_destry(pctx);
        return 0;
    }
    dbg("parsing ok.");
    if (ps->files_count >= ps->files_cap) {
        int new_cap = ps->files_cap ? ps->files_cap * 2 : 4; // start with 4 if 0
        Parser** new_files = realloc(ps->files, new_cap * sizeof(*ps->files));
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
    ps.files = malloc(10*sizeof(Parser*));
    ps.files_cap = 10;
    ps.files_count = 0;
    master_table.count = 0;
    master_table.cap = 10;
    // if it fails just segv at this point. idk
    master_table.symbols = malloc(master_table.cap*sizeof(Symbol*));
    master_table.parent= NULL;
    Arena a = arena_new(1024, sizeof(Symbol));
    master_table.arena = &a;

    add_base_types(&master_table);

    int errs = 0;
    for (int i = 0; paths[i] != 0 && i < 10; i++) {
        char* path = paths[i];
        dbg("file %s...", path);

        Parser* pctx = handle_new_file(&ps, path);
        if (!pctx) {
            err("Failed to parse file \"%s\".", path);
            errs++;
            continue;
        }
        // ps.files[ps.files_count++] = pctx;
        // codegen
    }
    // codegen
    for (int i = 0; i < ps.files_count; i++) {
        printf("generating pctx %d...\n", i);
        fflush(stdout);
        Parser* pctx = ps.files[i];
        /*if (!code_gen(pctx)) {
            err("Failed to generate file for %.*s",
                    (int)pctx->module_name.length,
                    pctx->module_name.name);
            return 1;
        }*/
        info("generated file %d\n", i);
    }
    // free
    for (int i = 0; i < ps.files_count; i++) {
        printf("freeing pctx %d...\n", i);
        fflush(stdout);
        Parser* pctx = ps.files[i];
        if (!parser_destry(pctx)) {
            err("Failed to free parser context");
            return 1;
        }
        info("freed file %d\n", i);
    }
    free(ps.files);
    for (int i = 0; i < a.pages_count; i++) {
        free(a.pages[i]);
    }
    free(a.pages);
    st_destroy(&master_table);
    if (open_paths) free(open_paths);

    printf("Finished.\n");
    return errs != 0;
}

