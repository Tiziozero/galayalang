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
        case tt_void: return "void";
        default:
            panic("unhandeled %d", t->kind);
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

typedef struct {
    enum { cgval, cgaddr } kind;
    const char* type;
    union {
        const char* val;
        const char* addr;
    };
    int ok;
}CGVal;
CGVal cg_lvalue(CGCtx* ctx, Node* n) {
    FILE* f = ctx->f;
    Arena* a = &ctx->p->arena;
    assert(f);
    int errs = 0;
    switch (n->kind) {
        case NodeSymbol:
            {
                Variable s = n->symbol->var;
                HMValue*  v = hashmap_get(&ctx->map, s.name);
                if (!v) {
                    panic("Variable %.*s not set?", s.name.length, s.name.name);
                    return (CGVal){.ok=0};
                }
                return (CGVal){.ok=1, .kind=cgaddr, .addr=v->ptr, .type=v->type};
            } break;
        default: panic("handle %s", NodeKindToString(n->kind)); return (CGVal){.ok=0};
    }
    panic("no");
    return (CGVal){.ok=0};
};
CGVal cg_expr(CGCtx* ctx, Node* n) {
    dbg("CG_EXPR %s", NodeKindToString(n->kind));
    FILE* f = ctx->f;
    Arena* a = &ctx->p->arena;
    assert(f);
    int errs = 0;
    switch (n->kind) {
        case NodeBinOp:
            {
                if (n->binop.type == OpAssign) {
                    // target
                    CGVal lhs = cg_lvalue(ctx, n->binop.left);
                    // value
                    CGVal rhs = cg_expr(ctx, n->binop.right);

                    assert(lhs.ok && rhs.ok);
                    assert(lhs.kind==cgaddr && rhs.kind == cgval);
                    const char* type = type_to_llvm_type(n->type);
                    fprintf(f, "store %s %s, %s* %s", type, rhs.val, type, lhs.addr);
                    return rhs; // return value
                }
                CGVal lhs = cg_expr(ctx, n->binop.left);
                CGVal rhs = cg_expr(ctx, n->binop.right);
                assert(lhs.ok && rhs.ok);
                assert(lhs.kind==cgval && rhs.kind == cgval);
                // temporary name
                const char* r_name = aprintf(a, "%%t%d", get_tmp_index());
                fprintf(f, "%s = ", r_name);
                switch (n->binop.type) {
                    case OpAdd: fprintf(f, "add"); break;
                    case OpSub: fprintf(f, "sub"); break;
                    default: panic("Handle");
                }
                const char* type = type_to_llvm_type(n->type);
                fprintf(f, " %s", type);
                fprintf(f, " %s,", lhs.val);// print left
                fprintf(f, " %s\n", rhs.val);// print right
                return (CGVal){.kind=cgval, .addr=r_name, .ok=1};
            } break;
        case NodeNumLit:
            {
                if (n->number.kind == NumKindInt) {
                    return (CGVal){
                        .ok=1,
                        .kind=cgval, 
                        .type= type_to_llvm_type(n->type),
                        .val=aprintf(a, "%lu", n->number.integer)};
                } else {
                    return (CGVal){
                        .ok=1,
                        .kind=cgval, 
                        .type= type_to_llvm_type(n->type),
                        .val=aprintf(a, "%lf", n->number.number)};
                }
            }break;
        case NodeSymbol: // vardecs are expressions too!
            {
                Variable s = n->symbol->var;
                HMValue*  v = hashmap_get(&ctx->map, s.name);
                if (!v) {
                    panic("Variable %.*s not set?", s.name.length, s.name.name);
                    return (CGVal){.ok=0};
                }
                const char* tmp_v = aprintf(a, "%%t%d", get_tmp_index());
                fprintf(f, "%s = load %s, %s* %s\n", tmp_v, v->type, v->type, v->ptr);
                return (CGVal){.kind=cgval, .val=tmp_v, .ok=1};
            } break;
        case NodeVarDec: // vardecs are expressions too!
            {   // %x will be a ptr, rather than of type
                Variable s = n->symbol->var;
                const char* ptr = aprintf(a, "%%%s", name_to_cstr(a, s.name));
                const char* type = type_to_llvm_type(s.type);
                fprintf(f, "%s = alloca %s\n", ptr, type);
                HMValue v;
                v.ptr = ptr;
                v.type = type;
                HMValue* hmvp = arena_alloc(a, sizeof(HMValue)); // arena alloc
                *hmvp = v;
                hashmap_set(&ctx->map, s.name, hmvp);
                if (n->var_dec.value) {
                    dbg("VARDEC HAS VALUYEW!!");
                    CGVal v = cg_expr(ctx, n->var_dec.value);
                    assert(v.ok && v.kind==cgval);
                    fprintf(f, "store %s %s, %s* %s\n", type, v.val, type, ptr);
                    return v;
                } else {
                    fprintf(f, "store %s 0, %s* %s\n", type, type, ptr);
                    return (CGVal){.ok = 1, .kind=cgval, .val="0", .type=type};
                }
            } break;
        case NodeFnCall: // %t0 = call i32 @add(i32 1, i32 2)
            {
                Variable fn = n->fn_call.target->symbol->var;
                assert(fn.type);
                assert(fn.type->kind == tt_ptr);
                assert(fn.type->ptr->kind == tt_fn); // ptr to fn
                const char* ret_t = type_to_llvm_type(fn.type->ptr->fn.return_type);
                const char* rname = aprintf(a, "%%t%d", get_tmp_index());
                fprintf(f,"%s = call %s @%s", rname, ret_t, name_to_cstr(a, fn.name));
                // args someday
                fprintf(f, "()\n");
                return (CGVal){.ok=1,.kind=cgval, .val=rname, .type=ret_t};
            } break;
        default: panic("handle %s", NodeKindToString(n->kind)); return (CGVal){.ok=0};
    }
    panic("no");
    return (CGVal){.ok=0};
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
                CGVal r = cg_expr(ctx, n->ret.expr);
                assert(r.ok && r.kind == cgval);
                const char* type = type_to_llvm_type(n->type);
                if (!type) {
                    panic("Failed to get llvm type from gala type. "
                            "This is a serious issue and means something's "
                            "wrong with the compiler.");
                    return 0;
                }
                fprintf(f, "ret %s %s ", type, r.val);
                dbg("Ret errs %d", errs);
            } break;
        case NodeBinOp:
        case NodeVarDec:
        case NodeSymbol:
        case NodeFnCall:
            if (!cg_expr(ctx, n).ok) {
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
