#include "lexer.h"
#include "parser.h"
#include <stdio.h>

int type_is_in_st(SymbolTable* st, Type* _t) {
    if (!_t){
        panic("No type.");
        return 0;
    }
    Type* t = _t;
    while (t->kind == tt_ptr || t->kind == tt_fn) {
        if (!t){
        panic("No type.");
        return 0;
    }
        if (t->kind == tt_ptr) {
            dbg("Is ptr");
            t = t->ptr;
        }
        if (t->kind == tt_fn) {
            dbg("Is fn");
            t = t->fn.return_type;
        }
    }
    print_type(t);
    printf(" ===\n");
    SymbolTable* s = st;
    if (!t) {
        panic("no t in type is in st");
        return 0;
    }
    while (s) {
        for (size_t i = 0; i < s->types_count; i++) {
            if (s->types[i] == t) return 1; // Type** so direct pointer compare
            // printf("Not %zu %zu", (size_t)s->types[i], (size_t)_t);
        }
        s = s->parent;
    }
    print_type(t);
    err("type not in tc");
    return 0;
}

int check_type(Parser* p, SymbolTable* st, Type* t, Token tok) {
    if (!t) {
        panic("No type.");
        return 0;
    }
    if (t->kind == tt_ptr)
        return check_type(p, st, t->ptr, tok);
    if (t->kind == tt_fn) {
        dbg("fn. ok");
        return 1;
    }
    if (t->kind == tt_to_determinate) {
        print_type(t);
        printf("\n");
        fflush(stdout);
        panic("Unresolved type '%.*s' — still tt_to_determinate after resolve.",
            (int)tok.ident.length, tok.ident.name);
        return 0;
    }
    if (!type_is_in_st(st, t)) {
        panic("Type* %p (kind %d) not in any symbol table.",
            t, t->kind);
        return 0;
    }
    return 1;
}

int node_all_good(Parser* p, SymbolTable* st, Node* n) {
    if (!n) return 1;
    int ok = 1;

    if (!n->resolved) panic("node not resolved.");

    if (!type_is_in_st(st, n->type)) {
        print_type(n->type);
        printf(" is not in symbol table (node %s Token %s).\n",
            NodeKindToString(n->kind), get_token_data(n->token));
        ok = 0;
    }

    switch (n->kind) {
        case NodeNone:
        case NodeEmpty:
            break;

        case NodeSymbol: {
            if (!n->symbol) {
                panic("Unresolved symbol '%.*s' (%zu %zu) doesn't exist.",
                    (int)n->ident.length, n->ident.name,
                    n->ident.length, n->token.ident.length);
                ok = 0;
            }
            break;
        }

        case NodeVarDec:
        case NodeConstDec: {
            if (n->var_dec.type)
                ok &= check_type(p, st, n->type, n->token);
            ok &= node_all_good(p, st, n->var_dec.value);
            break;
        }

        case NodeFnDec: {
            if (n->fn_dec.return_type)
                ok &= check_type(p, st, n->fn_dec.return_type->type_data, n->token);
            ok &= node_all_good(p, st, n->fn_dec.args);
            ok &= node_all_good(p, st, n->fn_dec.body);
            break;
        }

        case NodeArgs: {
            for (size_t i = 0; i < n->args.count; i++)
                ok &= node_all_good(p, st, n->args.args[i]);
            break;
        }

        case NodeArg: {
            if (n->arg.type)
                ok &= check_type(p, st, n->arg.type->type_data, n->token);
            break;
        }

        case NodeBlock: {
            for (size_t i = 0; i < n->block.count; i++)
                ok &= node_all_good(p, st, n->block.stmts[i]);
            // ok &= node_all_good(p, st, n->block.last); // when tail exprs land
            break;
        }

        case NodeIfStmt: {
            ok &= node_all_good(p, st, n->if_stmt.cond);
            ok &= node_all_good(p, st, n->if_stmt.block);
            if (n->if_stmt.else_block)
                ok &= node_all_good(p, st, n->if_stmt.else_block);
            for (size_t i = 0; i < n->if_stmt.alt_count; i++) {
                ok &= node_all_good(p, st, n->if_stmt.alt_conds[i]);
                ok &= node_all_good(p, st, n->if_stmt.alt_blocks[i]);
            }
            break;
        }

        case NodeRet:
            ok &= node_all_good(p, st, n->ret.expr);
            break;

        case NodeModuleAccess:
            ok &= node_all_good(p, st, n->module_access.module);
            break;

        case NodeBinOp:
            ok &= node_all_good(p, st, n->binop.left);
            ok &= node_all_good(p, st, n->binop.right);
            break;

        case NodeUnary:
            ok &= node_all_good(p, st, n->unary.target);
            break;

        case NodeFnCall:
            ok &= node_all_good(p, st, n->fn_call.target);
            ok &= node_all_good(p, st, n->fn_call.args);
            break;

        case NodeFieldAccess:
            ok &= node_all_good(p, st, n->field_access.target);
            break;

        case NodeIndex:
            ok &= node_all_good(p, st, n->index.target);
            ok &= node_all_good(p, st, n->index.index);
            break;

        case NodeCast:
            ok &= check_type(p, st, n->cast.to, n->token);
            ok &= node_all_good(p, st, n->cast.target);
            break;

        case NodeTypeData:
            ok &= check_type(p, st, n->type_data, n->token);
            break;

        case NodeNodeList: {
            for (size_t i = 0; i < n->node_list.count; i++)
                ok &= node_all_good(p, st, n->node_list.nodes[i]);
            break;
        }

        case NodeStringLit:
        case NodeNumLit:
            break; // nothing to recurse into

        case NodeStructLit: {
            for (size_t i = 0; i < n->struct_literal.count; i++)
                ok &= node_all_good(p, st, n->struct_literal.fields[i]->node);
            break;
        }

        case NodeCount:
            panic("NodeCount appeared in AST — this is a bug.");
            break;
    }

    dbg("%s ok? %d.", NodeKindToString(n->kind), ok);
    return ok;
}

int all_good(Parser* p) {
    int ok = 1;
    for (size_t i = 0; i < p->nodes_count; i++)
        ok += ! node_all_good(p, p->syms, p->nodes[i]);
    return ok;
}
