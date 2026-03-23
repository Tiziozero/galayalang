#include "logger.h"
#include "parser.h"
#include "utils.h"
int symbols(Parser* p, SymbolTable*s, Node* n);
int resolve_symbols(Parser* p) {
    if (!p) {
        return 0;
    }

    size_t errs= 0;
    for (size_t i = 0; i < p->nodes_count; i++) {
        if (!symbols(p, p->syms, p->nodes[i])) {
            err("Failed to resolve symbols for node %i.", i);
        }
    }
    return errs == 0;
}
int symbols(Parser* p, SymbolTable* st, Node* n) {
    if (!n) return 0;
    dbg("Node %d", n->kind);
    int errs = 0;
    for (int i = 0; i < sizeof(base_types) / sizeof(base_types[0]); i++) {
        Type t = base_types[i];
        // dbg("Base Type %.*s, (size %zu, type %d)...", (int)t.name.length, t.name.name, t.size, t.kind);
    }
    switch (n->kind) {
        case NodeVarDec:
            {
                dbg("Vardec.");
                if (!is_valid_name(n->var_dec.ident->ident)) {
                    panic("invalid name in vardec. shouldn't happen.");
                    return 0;
                }
                if (!n->var_dec.type) {
                    panic("No type. inference not implemented.");
                    return 0;
                }
                Type*t = st_resolve_type(st, n->var_dec.type->type_data);
                if (!t) {
                    err("Failed to resolve vardec type.");
                    return 0;
                }

                info("Resolved Type %.*s, (size %zu, kind %d)...", (int)t->name.length, t->name.name, t->size, t->kind);
                if (!type_is_in_st(st, t)) {
                    SymbolTable* s = st;
                    while (s) {

                        for (int i = 0; i < s->types_count; i++) {
                            print_type(s->types[i]);
                            printf("(%zu)\n", s->types[i]);
                            fflush(stdout);
                        }
                        s = s->parent;
                    }
                    panic("But type is not in st (%zu).",t);
                    return 0;
                }
                if (n->var_dec.value) {
                    if (!symbols(p, st, n->var_dec.value)) {
                        err("Failed to resolve symbols for vardec  value.");
                        return 0;
                    }
                }
                Variable v;
                v.name = n->var_dec.ident->ident;
                v.type = t; // resloved type
                Symbol* var_sym = st_add_var(p->syms, v);
                if (!var_sym) {
                    err("failed to create variable symbol.");
                    return 0;
                }
                n->symbol = var_sym;
                dbg("Vardec ok.");
                n->resolved = 1;
            } break;
        case NodeFnDec:
            {
                if (n->fn_dec.ident->kind != NodeSymbol) {
                    panic("FnDec symbol MUST be a symbol (identifier), got %d",
                            n->fn_dec.ident->kind);
                    return 0;
                }
                // some resumptions here
                if (st_sym_exists(st, n->fn_dec.ident->ident)) {
                    errs++;
                    err("Symbol for already exists.");
                }
                if (n->fn_dec.body) {
                    if (!symbols(p, st, n->fn_dec.body)) {
                        errs++;
                        err("Failed to symbol check fn body.");
                        return 0;
                    }
                }
                n->resolved = 1;
            } break;
        case NodeScope: // scopes
            {
                dbg("Scope %zu stmts.", n->scope.count);
                SymbolTable* scope = st_new(p, st);
                for (size_t i = 0; i < n->scope.count; i++) {
                    Node* stmt = n->scope.stmts[i];
                    if (!symbols(p, scope, stmt)) {
                        err("Failed to resolve statement symbol in scope.");
                        errs++;
                    }
                }
                // since symbols are now resolved (if successfule);
                st_destroy(scope);
            } break;
        case NodeSymbol:
            {
                dbg("Symbol.");
                Symbol* s = st_get_var(st, n->ident);
                if (!s) {
                    err("Symbol %.*s doesn't exist.",
                            (int)n->ident.length,
                            n->ident.name);
                    return 0;
                }
                n->symbol = s;
                n->resolved = 1;
            } break;
        case NodeUnary:
            dbg("Unary.");
            n->resolved = 1;
            return symbols(p, st, n->unary.target);
        case NodeNumLit:
            dbg("Numlit. ok");
            n->resolved = 1;
            return 1;
        case NodeBinOp: 
            {
                int a = 
                    symbols(p, st, n->binop.left)
                    && symbols(p, st, n->binop.right);
                dbg("Binop. %zu", a);
                n->resolved = a;
                return a;
            };
        case NodeRet:
            if (!symbols(p, st, n->ret.expr)) {
                err("Failed to resolve return expression symbols.");
                n->resolved = 0;
                return 0;
            }
            return 1;
        default: TODO("resolve symbol. %d", n->kind);
    }
    n->resolved = errs == 0;
    return errs == 0;
}
