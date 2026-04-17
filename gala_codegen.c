#include "constants.h"
#include "hash_map.h"
#include "logger.h"
#include "parse_number.h"
#include "parser.h"
#include "utils.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
typedef struct CGCtx CGCtx;
typedef struct GCNode GCNode;
typedef struct CGVar CGVar;
struct CGVar {
    Span name;
    int stack_offset;
};
struct CGCtx {
    FILE* f;
    Parser* p;
    int count, consts_count;
    HashMap map;
};
const char* aprintf(Arena* a, const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    va_list args_copy;
    va_copy(args_copy, args);

    int len = vsnprintf(NULL, 0, fmt, args_copy);
    if (len < 0) {
        panic("what");
        return NULL;
    }
    va_end(args_copy);

    char* buf = arena_alloc(a, len + 1);
    vsnprintf(buf, len + 1, fmt, args);
    va_end(args);
    return buf;
}
const char* type_to_llvm_type(Type* t) {
    switch (t->kind) {
        case tt_i32: return "i32";
        case tt_f32: return "float";
        case tt_ptr: return "ptr";
        default:
            panic("unhandeled");
            return NULL;
    }
}
int get_tmp_index() {
    static long index = 0;
    return (int)index++;
}
typedef struct {
    const char* ptr; // "%x"
    const char* type;
} HMValue;
const char* cg_expr(CGCtx* ctx, Node* n) {
    FILE* f = ctx->f;
    Arena* a = &ctx->p->arena;
    assert(f);
    int errs = 0;
    switch (n->kind) {
        case NodeBinOp:
            {
                const char* lhs = cg_expr(ctx, n->binop.left);
                const char* rhs = cg_expr(ctx, n->binop.right);
                const char* r_name = aprintf(a, "%%binop_res_%d", get_tmp_index());
                fprintf(f, "%s = ", r_name);
                switch (n->binop.type) {
                    case OpAdd: fprintf(f, "add"); break;
                    case OpSub: fprintf(f, "sub"); break;
                    default: panic("Handle");
                }
                const char* type = type_to_llvm_type(n->type);
                fprintf(f, " %s ", type);
                fprintf(f, " %s, ", lhs);// print left
                fprintf(f, " %s\n", rhs);// print right
                return r_name;
            } break;
        case NodeNumLit:
            {
                if (n->number.kind == NumKindInt) {
                    return aprintf(a, "%lu", n->number.integer);
                } else {
                    return aprintf(a, "%lf", n->number.number);
                }
            }break;
        case NodeVarDec: // vardecs are expressions too!
            {
                Variable s = n->symbol->var;
                const char* ptr = aprintf(a, "%%%s", name_to_cstr(a, s.name));
                const char* type = type_to_llvm_type(s.type);
                fprintf(f, "%s = alloca %s\n", ptr, type);
            } break;
        default: panic("handle %s", NodeKindToString(n->kind)); return 0;
    }
    panic("no");
    return NULL;
}
int cg_node(CGCtx* ctx, Node* n) {
    FILE* f = ctx->f;
    assert(f);
    int errs = 0;
    switch (n->kind) {
        // "define <type> @<fn name>(<type> %<arg_name>) {"
        case NodeFnDec:
            {
                assert(n->symbol->kind == SymObj);
                Variable s = n->symbol->var;
                // print to file
                // "define"
                fprintf(f, "define ");
                Type* ret_t = s.type->ptr->fn.return_type;
                const char* type = type_to_llvm_type(ret_t);
                if (!type) {
                    panic("Failed to get llvm type from gala type. "
                            "This is a serious issue and means something's "
                            "wrong with the compiler.");
                    return 0;
                }
                // <type>
                fprintf(f, "%s ", type);
                // "@"
                fprintf(f, "@");
                char* name = name_to_cstr(&ctx->p->arena, s.name);
                if (!name) {
                    panic("Failed to fn name. "
                            "This is a serious issue and means something's "
                            "wrong with the compiler.");
                    return 0;
                }
                // <fn name>
                fprintf(f, "%s", name);
                // args later maybe
                fprintf(f, "() {\n");
                // define entry
                fprintf(f, "entry:\n");
                // gen body
                cg_node(ctx, n->fn_dec.body);
                // finish
                fprintf(f, "}\n");
            } break;
        case NodeNodeList:
            {
                for (int i = 0; i < n->node_list.count; i++) {
                    errs += !cg_node(ctx, n->node_list.nodes[i]);
                    fprintf(f, "\n");
                }
            } break;
        case NodeBlock:
            {
                for (int i = 0; i < n->block.count; i++) {
                    if (!cg_node(ctx, n->block.stmts[i])) {
                        panic("Failed to get block stmt %d (%s)",
                                i, NodeKindToString(n->block.stmts[i]->kind));
                        errs++;
                    }
                    fprintf(f, "\n");
                }
            } break;
        case NodeRet:
            {
                const char* r = cg_expr(ctx, n->ret.expr);
                const char* type = type_to_llvm_type(n->type);
                if (!type) {
                    panic("Failed to get llvm type from gala type. "
                            "This is a serious issue and means something's "
                            "wrong with the compiler.");
                    return 0;
                }
                fprintf(f, "ret %s %s ", type, r);
                dbg("Ret errs %d", errs);
            } break;
        case NodeBinOp:
        case NodeVarDec:
        case NodeSymbol:
            if (!cg_expr(ctx, n)) {
                panic("Failed to gen expr.");
                return 0;
            }
            return 1;
        default: panic("handle %s", NodeKindToString(n->kind)); return 0;
    }
    if (errs != 0) {
        panic("Failed to codegen node %s (%d errors).",
                NodeKindToString(n->kind), errs);
        return 0;
    }
    return 1;
}
int cg_program(Parser* p) {
    static int count = 0;
    CGCtx ctx;
    memset(&ctx, 0, sizeof(CGCtx));

    char name[1024];
    int n = sprintf(name, "gala_llnv_mod_%s_%.5d.ll",
            name_to_cstr(&p->arena, p->module_name), count++);
    info("cg name \"%s\".", name);
    FILE* f = fopen(name, "wb");
    if (!f) {
        panic("Failed to open gala codegen file \"%s\".", name);
        return 0;
    }
    fprintf(f, "target triple = \"x86_64-pc-linux-gnu\"\n");
    ctx.f = f;
    ctx.p = p;
    Node* program = make_node_list(p, p->nodes, p->nodes_count);
    ctx.map = hashmap_new(&ctx.p->arena);

    int r = cg_node(&ctx, program); 
    info("r %d, writing file.", r);
    fclose(f);
    char cmd[1024];
    sprintf(cmd, "clang %s", name);
    int ret = system(cmd);
    sprintf(cmd, "./a.out");
    ret = system(cmd);
    info("%s returned %d. File:", name, ret);
    sprintf(cmd, "cat %s", name);
    ret = system(cmd);
    return r;
}
