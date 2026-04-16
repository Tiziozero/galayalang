#include "gala_vm.h"
#include "hash_map.h"
#include "parser.h"
#include "utils.h"
#include <assert.h>
#include <stdio.h>
typedef struct CGCtx CGCtx;
typedef struct GCNode GCNode;
struct GCNode {
    OpCode kind;
    union {
        int const_index; // index to constant in const table
    };
};
typedef struct CGVar CGVar;
struct CGVar {
    Span name;
    int stack_offset;
};
struct CGCtx {
    FILE* f;
    Parser* p;
    int count, consts_count;
    char cmp[VM_MEM];
    Arena arena;
    HashMap map;
};
int cg_node(CGCtx* ctx, Node* n) {
    FILE* f = ctx->f;
    int errs = 0;
    switch (n->kind) {
        default: panic("handle"); return 0;
    }
    return errs == 0;
}
int cg_program(Parser* p) {
    CGCtx ctx;

    char name[1024];
    int n = sprintf(name, "gala_llnv_mod_%s.ll", name_to_cstr(&p->arena, p->module_name));
    info("cg name \"%s\".", name);
    return 0;
    Node* program = make_node_list(p, p->nodes, p->nodes_count);
    memset(&ctx, 0, sizeof(CGCtx));
    ctx.arena = arena_new(1024, sizeof(CGVar));
    ctx.map = hashmap_new(&ctx.arena);

    return cg_node(&ctx, program);
}
