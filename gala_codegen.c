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
    CGCtx* parent;
    Arena a;
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
        case tt_u16:
        case tt_i16:
            return "i16";
        case tt_u32:
        case tt_i32:
            return "i32";
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
    enum { cgval, cgaddr } kind;
    const char* type;
    const char* val; // both addr and value atp
    int ok;
}CGVal;
typedef CGVal HMValue;
HMValue* cgctx_get_v(CGCtx* ctx, Span name) {
    HMValue* v = hashmap_get(&ctx->map, name);
    if (!v && ctx->parent)
        return hashmap_get(&ctx->parent->map, name);
    return v;
}
void cgctx_set_v(CGCtx* ctx, Span name, HMValue v) {
    HMValue* data = arena_alloc(&ctx->a, sizeof(HMValue));
    *data = v;
    hashmap_set(&ctx->map, name, data);
}
CGCtx* cgctx_new(CGCtx* parent) {
    CGCtx* ctx = malloc(sizeof(CGCtx));
    memset(ctx, 0, sizeof(CGCtx));
    if (parent) {
        ctx->parent = parent;
        ctx->p = parent->p;
        ctx->f = parent->f;
    }
    ctx->a = arena_new(1024, sizeof(CGVal));
    ctx->map = hashmap_new(&ctx->a);
    return ctx;
}
int cgctx_free(CGCtx* c) {
    for (int i = 0; i < c->a.pages_count; i++) {
        free(c->a.pages[i]);
    }
    free(c->a.pages);
    hashmap_free(&c->map);
    free(c);
    return 1;
}

CGVal cg_lvalue(CGCtx* ctx, Node* n) {
    FILE* f = ctx->f;
    Arena* a = &ctx->a;
    assert(f);
    int errs = 0;
    switch (n->kind) {
        case NodeSymbol:
            {
                Variable s = n->symbol->var;
                HMValue*  v = cgctx_get_v(ctx, s.name);
                if (!v) {
                    panic("Variable %.*s not set?", s.name.length, s.name.name);
                    return (CGVal){.ok=0};
                }
                info("lvalue check %.*s", s.name.length, s.name.name);
                assert(v->kind == cgaddr);
                return (CGVal){.ok=1, .kind=cgaddr, .val=v->val, .type=v->type};
            } break;
        default: panic("handle %s", NodeKindToString(n->kind)); return (CGVal){.ok=0};
    }
    panic("no");
    return (CGVal){.ok=0};
};
CGVal cg_expr(CGCtx* ctx, Node* n);
CGVal cg_cmp(CGCtx* ctx, Node* n) {
    assert(n->kind == NodeBinOp);

    FILE* f = ctx->f;
    Arena* a = &ctx->a;

    CGVal lhs = cg_expr(ctx, n->binop.left);
    CGVal rhs = cg_expr(ctx, n->binop.right);

    assert(lhs.ok && rhs.ok);
    assert(lhs.type && rhs.type);

    // ensure values (not addresses)
    const char* l = (lhs.kind == cgval) ? lhs.val : lhs.val;
    const char* r = (rhs.kind == cgval) ? rhs.val : rhs.val;

    const char* ty = lhs.type;
    const char* tmp = aprintf(a, "%%t%d", get_tmp_index());

    if (is_integer(n->type)) {
        const char* pred = NULL;

        switch (n->binop.type) {
            case OpEq:  pred = "eq"; break;
            case OpNeq: pred = "ne"; break;

            case OpLt:  pred = is_signed(n->type) ? "slt" : "ult"; break;
            case OpLe:  pred = is_signed(n->type) ? "sle" : "ule"; break;
            case OpGt:  pred = is_signed(n->type) ? "sgt" : "ugt"; break;
            case OpGe:  pred = is_signed(n->type) ? "sge" : "uge"; break;

            default: panic("invalid int cmp");
        }

        fprintf(f, "%s = icmp %s %s %s, %s\n", tmp, pred, ty, l, r);
    }
    else if (is_float(n->type)) {
        const char* pred = NULL;

        switch (n->binop.type) {
            case OpEq:  pred = "oeq"; break;
            case OpNeq: pred = "une"; break;

            case OpLt:  pred = "olt"; break;
            case OpLe:  pred = "ole"; break;
            case OpGt:  pred = "ogt"; break;
            case OpGe:  pred = "oge"; break;

            default: panic("invalid float cmp");
        }

        fprintf(f, "%s = fcmp %s %s %s, %s\n", tmp, pred, ty, l, r);
    }
    else {
        panic("unsupported type for comparison");
    }

    return (CGVal){
        .ok = 1,
        .kind = cgval,
        .val = tmp,
        .type = "i1"
    };
}
CGVal cg_expr(CGCtx* ctx, Node* n) {
    dbg("CG_EXPR %s", NodeKindToString(n->kind));
    FILE* f = ctx->f;
    Arena* a = &ctx->a;
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
                    fprintf(f, "store %s %s, %s* %s", type, rhs.val, type, lhs.val);
                    return rhs; // return value
                } else{
                    switch(n->binop.type) {
                        case OpGt:
                        case OpLt:
                        case OpGe:
                        case OpLe:
                        case OpEq:
                        case OpNeq:
                            return cg_cmp(ctx, n);
                        default: break;
                    }

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
                    case OpMlt: fprintf(f, "mul"); break;
                    case OpDiv: fprintf(f, "sdiv"); break;
                    default: panic("Handle");
                }
                const char* type = type_to_llvm_type(n->type);
                info("Node binop write");
                fprintf(f, " %s", type);
                fprintf(f, " %s,", lhs.val);// print left
                fprintf(f, " %s\n", rhs.val);// print right
                return (CGVal){.kind=cgval, .val=r_name, .type=type, .ok=1};
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
                HMValue*  v = cgctx_get_v(ctx, s.name);
                if (!v) {
                    panic("Variable %.*s not set?", s.name.length, s.name.name);
                    return (CGVal){.ok=0};
                }
                if (v->kind==cgaddr) {
                    info("Sym is a an addr");
                    const char* tmp_v = aprintf(a, "%%t%d", get_tmp_index());
                    fprintf(f, "%s = load %s, %s* %s\n", tmp_v, v->type, v->type, v->val);
                    return (CGVal){.kind=cgval, .val=tmp_v, .type=v->type, .ok=1};
                } else {
                    info("Sym is a val %zu", v->val);
                    return (CGVal){.kind=cgval, .val=v->val, .type=v->type, .ok=1};
                }
            } break;
        case NodeVarDec: // vardecs are expressions too!
            {   // %x will be a ptr, rather than of type
                Variable s = n->symbol->var;
                const char* ptr = aprintf(a, "%%%s", name_to_cstr(a, s.name));
                const char* type = type_to_llvm_type(s.type);
                fprintf(f, "%s = alloca %s\n", ptr, type);
                HMValue v;
                v.kind = cgaddr; // addr
                v.val = ptr;
                v.type = type;
                cgctx_set_v(ctx, s.name, v);
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
                const char* arg_s = NULL;
                // args
                if (n->fn_dec.args) {
                    // basic assertions
                    assert(n->fn_dec.args->kind == NodeNodeList);
                    for (int i = 0; i < n->fn_dec.args->node_list.count; i++) {
                        CGVal v = cg_expr(ctx, n->fn_dec.args->node_list.nodes[i]);
                        assert(v.ok && v.type);
                        if (arg_s != NULL){
                            if (v.kind == cgval)
                                arg_s = aprintf(a, "%s, %s %s",arg_s, v.type, v.val);
                            else
                                arg_s = aprintf(a, "%s, %s %s",arg_s, v.type, v.val);
                        } else {
                            if (v.kind == cgval)
                                arg_s = aprintf(a, "%s %s",v.type, v.val);
                            else
                                arg_s = aprintf(a, "%s %s",v.type, v.val);
                        }
                    }
                }
                const char* ret_t = type_to_llvm_type(fn.type->ptr->fn.return_type);
                const char* rname = aprintf(a, "%%t%d", get_tmp_index());
                fprintf(f,"%s = call %s @%s", rname, ret_t, name_to_cstr(a, fn.name));
                // args someday
                if (arg_s)
                    fprintf(f, "(%s", arg_s);
                else
                    fprintf(f, "(");
                fprintf(f,")\n");
                return (CGVal){.ok=1,.kind=cgval, .val=rname, .type=ret_t};
            } break;
        default: panic("handle %s", NodeKindToString(n->kind)); return (CGVal){.ok=0};
    }
    panic("no");
    return (CGVal){.ok=0};
}
int cg_node(CGCtx* ctx, Node* n) {
    FILE* f = ctx->f;
    Arena* a = &ctx->a;
    assert(f);
    int errs = 0;
    switch (n->kind) {
        // "define <type> @<fn name>(<type> %<arg_name>) {"
        case NodeFnDec:
            {
                info("FN DEC");
                assert(n->symbol->kind == SymObj);
                Variable s = n->symbol->var;
                // new ctx
                CGCtx* fn_ctx = cgctx_new(ctx); // will init with parent etc
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
                char* name = name_to_cstr(&ctx->a, s.name);
                if (!name) {
                    panic("Failed to fn name. "
                            "This is a serious issue and means something's "
                            "wrong with the compiler.");
                    return 0;
                }
                // <fn name>
                fprintf(f, "%s", name);
                // args later maybe
                fprintf(f, "(");
                if (n->fn_dec.args) {
                    assert(n->fn_dec.args->kind ==  NodeNodeList);
                    Node** nodes = s.type->ptr->fn.args->node_list.nodes;
                    int count = s.type->ptr->fn.args->node_list.count;
                    for (int i = 0; i < count; i++) {
                        // <type> %<name>
                        const char* t = type_to_llvm_type(nodes[i]->type);
                        const char* arg_name = name_to_cstr(a,
                                    nodes[i]->arg.ident->ident);
                        const char* name = aprintf(a, "%%%s", arg_name);
                        fprintf(f, "%s %s", t, name);
                        HMValue hmv;
                        memset(&hmv, 0, sizeof(HMValue));
                        hmv.kind = cgval;
                        hmv.val = name;
                        hmv.type = t;
                        info("%s (%.*s) %s for fn arg %d", name,
                                nodes[i]->arg.ident->ident.length,
                                nodes[i]->arg.ident->ident.name,
                                t, i);
                        cgctx_set_v(fn_ctx, nodes[i]->arg.ident->ident, hmv);
                        if (i != count-1) 
                            fprintf(f, ", ");
                    }
                }
                fprintf(f, ") {\n");
                // define entry
                fprintf(f, "entry:\n");
                // gen body
                cg_node(fn_ctx, n->fn_dec.body);
                // finish
                fprintf(f, "}\n");
                assert(cgctx_free(fn_ctx)); // make sure it frees correctly
                info("FN DEC END");
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
        case NodeForLoop:
            {
                int loop_index = get_tmp_index();
                const char* loop_cond_label = aprintf(a,
                        "loop%d.cond", loop_index);
                const char* loop_body_label = aprintf(a,
                        "loop%d.body", loop_index);
                const char* loop_end_label = aprintf(a,
                        "loop%d.end", loop_index);
                // llvm wants entry to jump here ig
                fprintf(f, "br label %%%s\n\n", loop_cond_label);
                fprintf(f, "%s:\n", loop_cond_label); // "loop0.cond:
                CGVal value = cg_expr(ctx, n->for_loop.cond);
                assert(value.ok);
                // comparision
                fprintf(f, "%%loop%d_cond_res = icmp ne %s %s, 0\n",
                            loop_index, value.type, value.val);
                // conditionally branch
                // br i1 %cond label %loop0.body, label %loop0.end
                fprintf(f, "br i1 %%loop%d_cond_res, label %%%s, label %%%s\n\n",
                                loop_index, loop_body_label, loop_end_label);
                fprintf(f, "%s:\n", loop_body_label); // "loop0.cond:
                cg_node(ctx, n->for_loop.block);
                // jump back to cond
                fprintf(f, "br label %%%s\n\n", loop_cond_label);
                fprintf(f, "%s:\n", loop_end_label); // "loop0.end: "end"

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
    CGCtx* ctx = cgctx_new(NULL);
    assert(ctx);

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
    ctx->f = f;
    ctx->p = p;
    Node* program = make_node_list(p, p->nodes, p->nodes_count);

    int r = cg_node(ctx, program); 
    info("r %d, writing file.", r);
    fclose(f);
    char cmd[1024*2];
    snprintf(cmd, 1024*2, "clang %s", name);
    int ret = system(cmd);
    snprintf(cmd, 1024*2, "./a.out");
    ret = system(cmd);
    info("%s returned %d. File:", name, ret);
    snprintf(cmd, 1024*2, "cat %s", name);
    ret = system(cmd);
    assert(cgctx_free(ctx));
    return r;
}
