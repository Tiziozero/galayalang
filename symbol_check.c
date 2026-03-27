#include "logger.h"
#include "parser.h"
#include "utils.h"
int symbols(Parser* p, SymbolTable*s, Node* n);
int resolve_symbols(Parser* p) {
    if (!p) {
        return 0;
    }

    int errs= 0;
    for (int i = 0; i < p->nodes_count; i++) {
        if (!symbols(p, p->syms, p->nodes[i])) {
            err("Failed to resolve symbols for node %i.", i);
        }
    }
    return errs == 0;
}
int symbols(Parser* p, SymbolTable* st, Node* n) {
    if (!n) return 0;
    dbg("Node %d %s", n->kind, NodeKindToString(n->kind));
    int errs = 0;
    for (int i = 0; i < sizeof(base_types) / sizeof(base_types[0]); i++) {
        Type t = base_types[i];
        // dbg("Base Type %.*s, (size %d, type %d)...", (int)t.name.length, t.name.name, t.size, t.kind);
    }
    switch (n->kind) {
        case NodeVarDec:
            {
                dbg("Vardec %.*s.", (int)n->var_dec.ident->ident.length, n->var_dec.ident->ident.name);
                if (!is_valid_name(n->var_dec.ident->ident)) {
                    panic("invalid name in vardec. shouldn't happen.");
                    return 0;
                }
                Type* t = NULL;
                if (!n->var_dec.type) {
                    // panic("No type. inference not implemented.");
                    // return 0;
                    if (!n->var_dec.value) {
                        panic("Must have value for type inference.");
                        return 0;
                    }
                    t = new_type(p);
                    t->kind = tt_to_determinate;
                } else {
                    t = st_resolve_type(st, n->var_dec.type->type_data);
                    if (!t) {
                        err("Failed to resolve vardec type.");
                        return 0;
                    }
                    info("Resolved Type %.*s, (size %d, kind %d)...", (int)t->name.length, t->name.name, t->size, t->kind);
                    if (!type_is_in_st(st, t)) {
                        SymbolTable* s = st;
                        while (s) {

                            for (int i = 0; i < s->types_count; i++) {
                                print_type(s->types[i]);
                                printf("(%zu)\n", (size_t)s->types[i]);
                                fflush(stdout);
                            }
                            s = s->parent;
                        }
                        panic("But type is not in st (%d).",t);
                        return 0;
                    }
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
                Symbol* var_sym = st_add_var(st, v);
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
                Symbol* fn_s = 0;
                if (!(fn_s = st_add_var(args_st, v))) {
                    panic("Failed to create fn var.");
                    return 0;
                }
                // resolve args and create in args_st
                if (!symbols(p, args_st,n->fn_dec.args)) {
                    panic("Failed to resolve fn dec args");
                    st_destroy(args_st);
                    return 0;
                }
                // create symbol for recursion.
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
        case NodeBlock: // scopes
            {
                dbg("Scope %d stmts.", n->block.count);
                SymbolTable* block = st_new(p, st);
                for (int i = 0; i < n->block.count; i++) {
                    info("\t\t=== BLOCK STMT %d ===", i);
                    Node* stmt = n->block.stmts[i];
                    if (!symbols(p, block, stmt)) {
                        err("Failed to resolve statement symbol in block.");
                        errs++;
                    }
                    info("\t\t=== BLOCK STMT %d END ===", i);
                }
                // since symbols are now resolved (if successfule);
                st_destroy(block);
                n->resolved = 1;
                // set last
                n->block.last = n->block.stmts[n->block.count-1];
                dbg("END SCOPE BLOCK");
                return 1;
            } break;
        case NodeIfStmt:
            {
                info(" ==== IF START ===");
                if (!symbols(p, st, n->if_stmt.cond)) {
                    panic("failed to symbol check if condition.");
                    return 0;
                }
                if (!symbols(p, st, n->if_stmt.block)) {
                    panic("failed to symbol check if block.");
                    return 0;
                }
                info(" ==== ALT START ===");
                for (int i = 0; i < n->if_stmt.alt_count; i++) {
                    if (!symbols(p, st, n->if_stmt.alt_conds[i])) {
                        panic("failed to symbol check "
                                "if else condition %d.", i);
                        return 0;
                    }
                    if (!symbols(p, st, n->if_stmt.alt_blocks[i])) {
                        panic("failed to symbol check if else block %d.", i);
                        return 0;
                    }
                }
                info(" ==== ELSE START ===");
                if (!symbols(p, st, n->if_stmt.else_block)) {
                    panic("failed to symbol check if block.");
                    return 0;
                }
                n->type = &st_get_type(st, cstr_to_name("void"))->type;
                info(" ==== IF END ===");
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
            errs += !symbols(p, st, n->unary.target);
        case NodeNumLit:
            dbg("Numlit. ok");
            n->resolved = 1;
            return 1;
        case NodeBinOp: 
            {
                int a = 
                    symbols(p, st, n->binop.left)
                    && symbols(p, st, n->binop.right);
                dbg("Binop. %d", a);
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
            for (int i = 0; i < n->args.count; i++)
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
                n->type = a.type; // set type for call type cmp
                n->resolved = 1;
                return 1;
            }
        case NodeNodeList:
            {
                for (int i = 0; i < n->node_list.count; i++) {
                    if (!symbols(p, st, n->node_list.nodes[i])) {
                        panic("Failed to resolve node list node %d.", i);
                        return 0;
                    }
                }
            } break;
        case NodeFnCall:
            {
                if (!symbols(p, st, n->fn_call.target)) {
                    panic("Failed to resolve dn call target.");
                    return 0;
                }
                /* Type* t = n->fn_call.target->type;
                if (!t) {
                    panic("Failed to get target type st.");
                    return 0;
                }
                t = st_resolve_type(st, t);
                if (!t) {
                    panic("Failed to resolve target type.");
                    return 0;
                    n->fn_call.target->type = t;
                } */
                if (n->fn_call.args) {
                    if (!symbols(p, st, n->fn_call.args)) {
                        panic("Failed to resolve symbols for fn call args.");
                        return 0;
                    }
                }
            } break;
        case NodeCast:
            {
                info("Target kind %d", n->cast.target->kind);
                if (!symbols(p, st, n->cast.target)) {
                    panic("Failed to resolve cast target symbols.");
                    return 0;
                }
                Type* t = st_resolve_type(st, n->cast.to);
                if (!t) {
                    panic("Failed to resolve cast type.");
                    return 0;
                }
                n->cast.to = t; // always update, bud
            } break;
        case NodeStructDec:
            {
                Span name = n->struct_dec.ident->ident;
                if (st_sym_exists(st, name)) {
                    panic("symbol already exists %s.", name.length, name.name);
                    return 0;
                }
                int cap = 0, count = 0;
                Field* fields = calloc(1, cap*sizeof(Node*));
                int t_size = 0;
                for (int i = 0; i < n->struct_dec.count; i++) {
                    Span name = n->struct_dec.fields[i]->field_dec.ident->ident;
                    if (!is_valid_name(name)) {
                        panic("Invalid name in field dec %d", i);
                        return 0;
                    }
                    Type* t =
                        n->struct_dec.fields[i]->field_dec.type->type_data;
                    if (!t) {
                        panic("no type in struct dec field %d", i);
                        return 0;
                    }
                    t = st_resolve_type(st, t);
                    if (!t) {
                        panic("Faild to resolve st type.");
                        return 0;
                    }
                    t_size += t->size;
                    dbg("t_size %d", t->size);
                    Field f;
                    f.type = t;
                    f.name = name;
                    fields[count++] = f;
                }
                if (t_size == 0) {
                    panic("can't have empty struct.");
                    return 0;
                }
                Type t;
                t.kind = tt_struct;
                t.name = n->struct_dec.ident->ident;
                t.size = t_size;
                dbg("%d struct size.", t.size);
                t.struct_t.name = n->struct_dec.ident->ident;
                t.struct_t.fields = arena_alloc(&p->arena, sizeof(Node*)* count);
                memcpy(t.struct_t.fields,fields, sizeof(Node*)* count);
                free(fields);
                t.struct_t.count = count;
                Symbol* struct_t = st_add_type(st, t);
                n->symbol = struct_t;
                n->type = &struct_t->type;
            } break;
        case NodeStructLit:
            {
                Symbol* st_type = st_get_type(st,
                         n->struct_literal.type_name->ident);
                if (!st_type) {
                    panic("failed to get st struc ttype/type doesn't exist.");
                    return 0;
                }
                if (st_type->kind != SymType) {
                    panic("struct lit sym is not a type");
                    return 0;
                }
                if (st_type->type.kind != tt_struct) {
                    panic("struct lit type is not a struct.");
                    return 0;
                }
                StructType ref = st_type->type.struct_t;
                for (int i = 0; i < n->struct_literal.count; i++) {
                    Node* f = n->struct_literal.fields[i];
                    if (!symbols(p, st, f->named_field.expr)) {
                        panic("Failed to resolve struct lit field %d expr.", i);
                        return 0;
                    }
                    // check if it exists first
                    int exists = 0;
                    for (int j = 0; j < ref.count; j++) {
                        dbg("\tcheckig agains %.*s", 
                                ref.fields[i].name.length,
                                ref.fields[i].name.name);
                        if (name_cmp(ref.fields[i].name,
                                    f->named_field.ident->ident)) {
                            dbg("Exists.");
                            exists = 1;
                        }
                    }
                    if (!exists) {
                        panic("field %.*s doesn't exist in ref.",
                                f->named_field.ident->ident.length,
                                f->named_field.ident->ident.name);
                        return 0;
                    }
                }
                n->symbol = st_type;
                n->type = &st_type->type;
            } break;
        case NodeNone:
            panic("no.");
        default: TODO("resolve symbol. %d %s", n->kind, NodeKindToString(n->kind));
    }
    n->resolved = errs == 0;
    return errs == 0;
}
