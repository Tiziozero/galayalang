#include "lexer.h"
#include "parser.h"
#include <stdio.h>

int type_is_in_st(SymbolTable* st, Type* t) {
    if (!t) { panic("No type."); return 0; }

    // unwrap ptr/fn chains
    Type* inner = t;
    while (inner && (inner->kind == tt_ptr || inner->kind == tt_fn)) {
        if (inner->kind == tt_ptr) inner = inner->ptr;
        else                       inner = inner->fn.return_type;
    }
    if (!inner) { panic("Null type after unwrap."); return 0; }

    if (!type_registry_contains(inner)) {
        panic("Type* %p (kind=%d name=%.*s) not in global type registry.",
            inner, inner->kind,
            (int)inner->name.length, inner->name.name);
        return 0;
    }
    return 1;
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

    if (!n->resolved) {
        panic("node not resolved: %s", NodeKindToString(n->kind));
        return 0;
    }

    switch (n->kind) {
        case NodeNone:
            panic("NodeNone in AST.");
            return 0;

        case NodeEmpty:
            break;

        case NodeSymbol:
            if (!n->symbol) {
                panic("Unresolved symbol '%.*s'.",
                    (int)n->ident.length, n->ident.name);
                ok = 0;
            }
            break;

        case NodeVarDec:
        case NodeConstDec:
            if (n->type) ok &= check_type(p, st, n->type, n->token);
            ok &= node_all_good(p, st, n->var_dec.ident);
            if (n->var_dec.type)  ok &= node_all_good(p, st, n->var_dec.type);
            if (n->var_dec.value) ok &= node_all_good(p, st, n->var_dec.value);
            break;

        case NodeFnDec:
            if (n->fn_dec.return_type)
                ok &= check_type(p, st, n->fn_dec.return_type->type_data, n->token);
            ok &= node_all_good(p, st, n->fn_dec.ident);
            if (n->fn_dec.args)   ok &= node_all_good(p, st, n->fn_dec.args);
            if (n->fn_dec.body)   ok &= node_all_good(p, st, n->fn_dec.body);
            break;

        case NodeArg:
            if (!n->symbol) { panic("Arg has no symbol."); ok = 0; }
            if (n->arg.type) ok &= check_type(p, st, n->arg.type->type_data, n->token);
            break;

        case NodeBlock:
            for (int i = 0; i < n->block.count; i++)
                ok &= node_all_good(p, st, n->block.stmts[i]);
            break;

        case NodeIfStmt:
            ok &= node_all_good(p, st, n->if_stmt.cond);
            ok &= node_all_good(p, st, n->if_stmt.block);
            if (n->if_stmt.else_block)
                ok &= node_all_good(p, st, n->if_stmt.else_block);
            for (int i = 0; i < n->if_stmt.alt_count; i++) {
                ok &= node_all_good(p, st, n->if_stmt.alt_conds[i]);
                ok &= node_all_good(p, st, n->if_stmt.alt_blocks[i]);
            }
            break;

        case NodeRet:
            if (n->ret.expr) ok &= node_all_good(p, st, n->ret.expr);
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
            if (n->fn_call.args) ok &= node_all_good(p, st, n->fn_call.args);
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

        case NodeNodeList:
            for (int i = 0; i < n->node_list.count; i++)
                ok &= node_all_good(p, st, n->node_list.nodes[i]);
            break;

        case NodeStringLit:
            break;

        case NodeNumLit:
            break;

        case NodeStructLit:
            if (!n->symbol) { panic("StructLit has no symbol."); ok = 0; }
            ok &= check_type(p, st, n->type, n->token);
            // fields is a NodeNodeList
            if (n->struct_literal.fields)
                ok &= node_all_good(p, st, n->struct_literal.fields);
            break;

        case NodeStructDec:
            if (!n->symbol) { panic("StructDec has no symbol."); ok = 0; }
            ok &= check_type(p, st, n->type, n->token);
            // field_decs is a NodeNodeList
            if (n->struct_dec.field_decs)
                ok &= node_all_good(p, st, n->struct_dec.field_decs);
            break;

        case NodeFieldDec:
            if (n->field_dec.type)
                ok &= check_type(p, st, n->field_dec.type->type_data, n->token);
            break;

        case NodeNamedField:
            ok &= node_all_good(p, st, n->named_field.ident);
            ok &= node_all_good(p, st, n->named_field.expr);
            break;

        case NodeCount:
            panic("NodeCount in AST — bug.");
            ok = 0;
            break;

        default:
            panic("Unhandled node in node_all_good: %d %s",
                n->kind, NodeKindToString(n->kind));
            ok = 0;
            break;
    }

    dbg("%s ok=%d", NodeKindToString(n->kind), ok);
    return ok;
}

int all_good(Parser* p) {
    int ok = 1;
    for (int i = 0; i < p->nodes_count; i++)
        ok += ! node_all_good(p, p->syms, p->nodes[i]);
    return ok;
}
