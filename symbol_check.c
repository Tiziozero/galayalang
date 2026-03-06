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

                info("Resolved Type %.*s, (size %zu, type %d)...", (int)t->name.length, t->name.name, t->size, t->kind);
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
                if (!symbols(p, st, n->fn_dec.fn_body)) {
                    errs++;
                    err("Failed to symbol check fn body.");
                    return 0;
                }
            } break;
        case NodeFn:
            {
                dbg("Fn %zu args.", n->fn_body.count);
                // args
                for (size_t i = 0; i < n->fn_body.count; i++) {
                    FnDecArg arg = n->fn_body.args[i];
                    Type* t = st_resolve_type(st, arg.type->type_data);
                    if (!t) {
                        err("Failed to resolve fn dec arg.");
                        errs++;
                    }
                }
                if (!symbols(p, st, n->fn_body.body)) {
                    err("Failed to resolve fn body symbols.");
                    errs++;
                }
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
                n->var = &s->var;
            } break;
        case NodeUnary:
            dbg("Unary.");
            return symbols(p, st, n->unary.target);
        case NodeNumLit:
            dbg("Numlit. ok");
            return 1;
        case NodeBinOp: 
            {
                int a = 
                    symbols(p, st, n->binop.left)
                    && symbols(p, st, n->binop.right);
                dbg("Binop. %zu", a);
                return a;
            };
        default: TODO("resolve symbol. %d", n->kind);
    }
    return errs == 0;
}
