#include "lexer.h"
#include "parser.h"
#include <stdio.h>

int type_is_in_st(SymbolTable* st, Type* _t) {
    if (!_t) return 0;
    Type* t = _t;
    while (t->kind == tt_ptr || t->kind == tt_fn) {
        if (!t) return 0;
        if (t->kind == tt_ptr) {
            t = t->ptr;
        }
        if (t->kind == tt_fn) {
            t = t->fn.return_type;
        }
    }
    if (!t) return 0;
    SymbolTable* s = st;
    while (s) {
        for (size_t i = 0; i < s->types_count; i++) {
            if (s->types[i] == t) return 1; // Type** so direct pointer compare
        }
        s = s->parent;
    }
    return 0;
}

int check_type(Parser* p, SymbolTable* st, Type* t, Token tok) {
    if (!t) return 0;
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
    if (!type_is_in_st(st, n->type)) {
        print_type(n->type);
        printf(" is  not in symbol table (node %s Token %s).\n",NodeKindToString(n->kind), get_token_data(n->token));
        ok = 0;
    }
    switch (n->kind) {
        case NodeSymbol: {
            Symbol* sym = n->symbol; // not st since they get freed
            if (!sym) {
                panic("Unresolved symbol '%.*s' (%.*s) (%zu %zu)(%zu %zu) doesn't exist.",
                    (int)n->ident.length, n->ident.name,
                    (int)n->token.ident.length, n->token.ident.name,
                    n->ident.length, n->ident.name,
                    n->token.ident.length, n->token.ident.name);
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
            // don't really care about ident since it's there just for name, like vardec
            // ok &= node_all_good(p, st, n->fn_dec.ident);
            if (n->fn_dec.return_type) // fn ret type is handeled in type check
                ok &= check_type(p, st, n->fn_dec.return_type->type_data, n->token);
            /*

            for (size_t i = 0; i < n->fn_dec.count; i++)
                ok &= check_type(p, st, n->fn_dec.args[i].type->type_data, n->token);
                */
            // body gets its own st — if you attach st to NodeScope, pass it here
            ok &= node_all_good(p, st, n->fn_dec.body);
            break;
        }
        case NodeScope: {
            // scopes have their own st — if you store it on the node, use it
            // for now fall back to parent st
            for (size_t i = 0; i < n->scope.count; i++)
                ok &= node_all_good(p, st, n->scope.stmts[i]);
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
            for (size_t i = 0; i < n->fn_call.args_count; i++)
                ; // ok &= node_all_good(p, st, n->fn_call.args[i]);
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
        default:
            break;
    }
    return ok;
}

int all_good(Parser* p) {
    int ok = 1;
    for (size_t i = 0; i < p->nodes_count; i++)
        ok &= node_all_good(p, p->syms, p->nodes[i]);
    return ok;
}
