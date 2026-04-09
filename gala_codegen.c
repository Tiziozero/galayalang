// gala_codegen.c
// Walks the resolved/typechecked AST and emits bytecode for the VM.
// Assumes all nodes have n->resolved=1 and n->type set.
//
// Usage:
//   Codegen cg;
//   cg_init(&cg, vm);
//   cg_program(&cg, root_node_list);

#include "gala_vm.h"
#include "hash_map.h"
#include "parse_number.h"
#include "parser.h"
#include "utils.h"
#include <assert.h>
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
    GCNode data[1024]; // for now
    Value value_consts[1024]; // for now
    int count, consts_count;
    Arena arena;
    HashMap map;
};
int cg_node(CGCtx* ctx, Node* n) {

    int errs = 0;
    switch (n->kind) {
        case NodeNodeList:
            {
                for (int i = 0; i < n->node_list.count; i++) {
                    errs += !cg_node(ctx, n->node_list.nodes[i]);
                }
            } break;
        case NodeNumLit: // push const
            {
                Value v;
                GCNode gcn;
                if (n->number.kind == NumKindInt) {
                    v.kind = VAL_I64;
                    v.i = n->number.integer;
                    gcn.kind = OP_CONST_I;
                } else if (n->number.kind == NumKindFloat) {
                    v.kind = VAL_F64;
                    v.f = n->number.number;
                    gcn.kind = OP_CONST_F;
                } else {
                    panic("Unknown number literal.");
                    return 0;
                }
                int index = ctx->consts_count;
                ctx->value_consts[ctx->consts_count++] = v;
                gcn.const_index = index;
                ctx->data[ctx->count++] = gcn;
            } break;
        case NodeSymbol:
            {
                // use symbol name
                Span name = n->symbol->name;
                CGVar* data = hashmap_get(&ctx->map, name);
                if (!data) {
                    panic("no symbol %.*s", name.length, name.name);
                    return 0;
                }

            } break;
        default: panic("handle"); return 0;
    }
    return errs == 0;
}
int cg_program(VM* vm, Node* root) {
    
    CGCtx ctx;

    memset(&ctx, 0, sizeof(CGCtx));
    ctx.arena = arena_new(1024, sizeof(CGVar));
    ctx.map = hashmap_new(&ctx.arena);

    return cg_node(&ctx, root);
}
