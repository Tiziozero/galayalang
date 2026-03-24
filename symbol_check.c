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
                // some asumptions here
                if (st_sym_exists(st, n->fn_dec.ident->ident)) {
                    errs++;
                    err("Symbol for already exists.");
                }
                Type fn;
                fn.kind = tt_ptr;
                fn.ptr = new_type(p);
                if (!fn.ptr) {
                    panic("Just kys atp bru.");
                    return 0;
                }
                fn.ptr->kind = tt_fn;
                fn.ptr->fn.args = n->fn_dec.args;
                // return type
                if (!n->fn_dec.return_type) { // set to void
                    dbg("Ret type void.");
                    // let it segfault
                    Symbol* v = st_get_type(st, cstr_to_name("void"));
                    if (!v) {
                        panic("No void ig.");
                        return 0;
                    } // set fn type ptr fn ret type
                    fn.ptr->fn.return_type = &v->type;
                } else { // resolvee
                    Type* resolved  =st_resolve_type(st, n->fn_dec.return_type->type_data);
                    if (!resolved) {
                        panic("FAield to resolve fn return type.");
                        return 0;
                    }
                    info("FN RET TYPE.");
                    fn.ptr->fn.return_type = resolved;
                    print_type(fn.ptr->fn.return_type);
                    printf("\n");
                    // set to resolved type
                    n->fn_dec.return_type->type_data = resolved;
                }
                // create type
                Type* fn_t = new_type(p);
                if (!fn_t) {
                    panic("Failed to allocate memory for new type.");
                    return 0;
                }
                *fn_t = fn;
                Variable v;
                v.name = n->fn_dec.ident->ident; // must be ident/symbol
                v.type = fn_t;

                // once created variable holding it, check body.
                SymbolTable* args_st = st_new(p, st);
                if (!args_st) {
                    panic("Failed to create args st.");
                    return 0;
                }
                // resolve args and create in args_st
                if (!symbols(p, args_st,n->fn_dec.args)) {
                    panic("Failed to resolve fn dec args");
                    st_destroy(args_st);
                    return 0;
                }
                // create symbol for recursion.
                Symbol* fn_s = 0;
                if (!(fn_s = st_add_var(st, v))) {
                    panic("Failed to create fn var.");
                    return 0;
                }

                if (n->fn_dec.body) {
                    if (!symbols(p, args_st, n->fn_dec.body)) {
                        errs++;
                        err("Failed to symbol check fn body.");
                        return 0;
                    }
                }
                // free args st
                st_destroy(args_st);
                n->symbol = fn_s; // set symbol
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
                n->resolved = 1;
                n->type = &st_get_type(st, cstr_to_name("void"))->type;
                return 1;
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
            if (!n->ret.expr) {
                n->type = &st_get_type(st, cstr_to_name("void"))->type;
                return 1;
            }
            if (!symbols(p, st, n->ret.expr)) {
                err("Failed to resolve return expression symbols.");
                n->resolved = 0;
                return 0;
            }
                n->resolved = 1;
            return 1;
        case NodeArgs:
            for (size_t i = 0; i < n->args.count; i++)
                errs += !symbols(p, st, n->args.args[i]);
            n->type = &st_get_type(st, cstr_to_name("void"))->type;
            break;
        case NodeArg:
            {
                if (!is_valid_name(n->arg.ident->ident)) {
                    panic("Invalid name in arg symmbol check.");
                    return 0;
                }
                Type* t = st_resolve_type(st, n->arg.type->type_data);
                if (!t) {
                    panic("Failed to resolve arg type.");
                    return 0;
                }
                Argument a;
                a.type = t;
                a.name = n->arg.ident->ident;
                Symbol* s = st_add_var(st, a);
                if (!s) {
                    panic("Failed to crate arg.");
                    return 0;
                }
                n->symbol = s;
                n->resolved = 1;
                return 1;
            }

        default: TODO("resolve symbol. %d %s", n->kind, NodeKindToString(n->kind));
    }
    n->resolved = errs == 0;
    return errs == 0;
}
