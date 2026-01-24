#include "parser.h"

// this file is a mess
Token current(ParserCtx* pctx) {
    Token t;
    if (pctx->tokens_index >= pctx->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = pctx->tokens[pctx->tokens_index];
        // pctx->tokens_index += 1;
    }
    return t;
}
Token peek(ParserCtx* pctx) {
    Token t;
    if (pctx->tokens_index + 1 >= pctx->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = pctx->tokens[pctx->tokens_index + 1];
    }
    return t;
}
Token consume(ParserCtx* pctx) {
    Token t;
    if (pctx->tokens_index >= pctx->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = pctx->tokens[pctx->tokens_index];
        // info("\t\tconsume %s", get_token_data(t));
        pctx->tokens_index += 1;
    }
    return t;
}

// returns 0 on success cus errors
SymbolType ss_sym_exists(SymbolStore* ss, Name name) {
    char buf[100];
    if (name.name == 0 || name.length == 0) return SymNone;
    // info("checking type %zu %zu", name.name, name.length);
    print_name_to_buf(buf, 100, name);
    // info("Checking name \"%s\"...", buf);
    Symbol* syms = ss->syms;
    for (size_t i = 0; i < ss->syms_count; i++) {
        if (syms[i].sym_type == SymType) {
            if (name_cmp(syms[i].type.name, name)) {
                // info("type %s exists", buf);
                return SymType;
            }
        } else if (syms[i].sym_type == SymVar) {
            if (name_cmp(syms[i].var.name, name)) {
                // info("var %s exists", buf);
                return SymVar;
            }
        } else if (syms[i].sym_type == SymFn) {
            if (name_cmp(syms[i].fn.name, name)) {
                // info("fn %s exists", buf);
                return SymFn;
            }
        } else if (syms[i].sym_type == SymArg) {
            if (name_cmp(syms[i].argument.name, name)) {
                // info("arg %s exists", buf);
                return SymArg;
            }
        } else if (syms[i].sym_type == SymField) {
            if (name_cmp(syms[i].field.name, name)) {
                // info("field %s exists", buf);
                return SymField;
            }
        } else {
            err("Invalid symbol type: %s.", syms[i].sym_type);
            assert(0);
            return SymNone;
        }
    }
    if (ss->parent) {
        SymbolType t = ss_sym_exists(ss->parent, name);
        if (t != SymNone) {
            return t;
        }
    }
    /* info("\"%s\" doesn't exist in %zu symbols.", buf, ss->syms_count);
    for (size_t i = 0; i < ss->syms_count; i++) {
        if (ss->syms[i].sym_type == SymVar) {
            printf("\tVar : "); print_name(ss->syms[i].var.name);
        }
        if (ss->syms[i].sym_type == SymType) {
            printf("\tType: "); print_name(ss->syms[i].type.name);
        }
    } */
    return SymNone;
}
// get_lowest_type_from_arr_or_ptr
Type* get_lowest_type(Type* type) {
    Type* _t = type;
    while(_t->type == tt_array || _t->type == tt_ptr) {
        if(_t->type == tt_array && _t->ptr) { // array is same as ptr
                                              // TODO: change once thing woks
            _t = _t->ptr;
        } else if(_t->type == tt_ptr && _t->ptr) {
            _t = _t->ptr;
        } else {
            err("pointer/array but type is null.");
            return NULL;
        }
    }
    return _t;
}
// returns 1 on success
int ss_new_var(SymbolStore* ss, Variable var) {
    Type* original_type = var.type;
    // pre-requirements
    if (var.type->type == tt_to_determinate) {
        err("Type is still to determinate");
        return 0;
    }
    if (var.name.name == 0 || var.name.length == 0) {
        err("Invalid name for var");
        return 0;
    }
    char name_buf[100];
    print_name_to_buf(name_buf, 100, var.name);
    char type_buf[100];
    print_name_to_buf(type_buf, 100, get_lowest_type(var.type)->name);

    // check if it exists
    if (ss_sym_exists(ss, var.name)) {
        err("var exists");
        return 0;
    }
    Type* check_type = var.type;
    while (check_type->type == tt_ptr || check_type->type == tt_array) {
        if (check_type->type == tt_ptr) {
            check_type = check_type->ptr;
        } else if (check_type->type == tt_array) {
            check_type = check_type->static_array.type;
        }
        if (!(check_type->type == tt_ptr || check_type->type == tt_array)) {
            char name_buf[100];
            print_name_to_buf(name_buf, 100, check_type->name);
        } 
    }
    SymbolType res = ss_sym_exists(ss, check_type->name);
    if (res != SymType) {
        char buf[100];
        print_name_to_buf(buf, 100, var.type->name);
        err(" === Undefined type \"%s\". === ", buf);
        if (res != SymNone) {
            info("\tgot %s instead.", res);
        } else {
            info("\t%s does not exist.", buf);
        }
        return 0;
    }
    if (ss->syms_count >= ss->syms_capacity) {
        info("more memory required for symbols");
        ss->syms = (Symbol*)realloc(
            ss->syms, (ss->syms_capacity*=2)*sizeof(Symbol));
        if (!ss->syms) {
            err("Failed to realloc memory for symbol store.");
            assert(0);
            return 0;
        }
    }
    ss->syms[ss->syms_count++] = (Symbol){
        .sym_type = SymVar,
        .var=var,
    };
    return 1;
}
int ss_new_type(SymbolStore* ss, Type t) {
    // check if it exists
    if (ss_sym_exists(ss, t.name)) return 0;
    if (ss->syms_count >= ss->syms_capacity) {
        info("more memory required for symbols");
        ss->syms = (Symbol*)realloc(
            ss->syms, (ss->syms_capacity*=2)*sizeof(Symbol));
        if (!ss->syms) {
            err("Failed to realloc memory for symbol store.");
            assert(0);
            return 2;
        }
    }
    ss->syms[ss->syms_count++] = (Symbol){
        .sym_type = SymType,
        .type=t
    };
    return 0;
}
int ss_new_fn(SymbolStore* ss, Function fn) {
    // check if it exists
    if (ss_sym_exists(ss, fn.name)) return 0;
    if (ss->syms_count >= ss->syms_capacity) {
        info("more memory required for symbols");
        ss->syms = (Symbol*)realloc(
            ss->syms, (ss->syms_capacity*=2)*sizeof(Symbol));
        if (!ss->syms) {
            err("Failed to realloc memory for symbol store.");
            assert(0);
            return 0;
        }
    }
    ss->syms[ss->syms_count++] = (Symbol){
        .sym_type = SymFn,
        .fn=fn,
    };
    /*fn = *ss_get_fn(ss, fn.name);
    info("fn args: %d.", fn.args_count);
    if (fn.args_count > 0) {
        info("arg 0 type %zu.", fn.args[0].type->type);
    } */
    return 1;
}
Function* ss_get_fn(SymbolStore* ss, Name name) {
    // check if it exists
    if (!ss_sym_exists(ss, name)) return NULL;
	for (size_t i = 0; i < ss->syms_count; i++) {
		Symbol* s = &ss->syms[i];
		if (s->sym_type == SymFn) {
			/* info("comparing %.*s (name) %.*s.",
					(int)name.length, name.name,
					(int)s->fn.name.length, s->fn.name.name); */
			if (name_cmp(name, s->fn.name)) {
				return &ss->syms[i].fn; // return reference to fn
			}
		}
	}
    if (ss->parent != NULL) {
        return ss_get_fn(ss->parent, name);
    }
    return 0;
}

ParserCtx* pctx_new(Token* tokens, size_t tokens_count) {
    ParserCtx* pctx = (ParserCtx*)malloc(sizeof(ParserCtx)); 
    if (!pctx) return NULL;
    // create arena
    Arena* arena = (Arena*)malloc(sizeof(Arena));
    if (!arena) { 
        free(pctx);
        return NULL;
    }
    *arena = arena_new(1024, sizeof(Node));
    // create ast
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->max_nodes=1024;
    ast->nodes = (Node**)malloc(sizeof(Node*)*ast->max_nodes);
    ast->nodes_count = 0;
    ast->arena = arena;
    pctx->ast = ast;

    SymbolStore ss;
    ss.syms_capacity = 256;
    ss.syms = (Symbol*)malloc(ss.syms_capacity*sizeof(Symbol));
    if (!ss.syms) {
        free(ast);
        free(arena);
        free(pctx);
        return NULL;
    }
    ss.syms_count = 0;
    ss.parent = NULL;

    pctx->symbols = ss;
    pctx->gpa = arena_new(1024, sizeof(void*));

    pctx->tokens = tokens;
    pctx->tokens_count = tokens_count;
    pctx->tokens_index = 0;



    // init known types
    for (size_t i = 0; i < sizeof(base_types)/sizeof(base_types[0]); i++) {
        char buf[100];
        print_name_to_buf(buf, 100, base_types[i].name);
        if (ss_new_type(&pctx->symbols, base_types[i])) {
            err("Failed to add type: %s", buf);
            assert(0);
        }
    }
    return pctx;
}
// returns 1 on success
int pctx_destry(ParserCtx* pctx) {
    info("Freeing pctx");
    // free node data first
    for (size_t i = 0; i < pctx->ast->nodes_count; i++) {
        Node* n = pctx->ast->nodes[i];
        if (n->kind == NodeFnDec) {
            if (n->fn_dec.body) 
                if (n->fn_dec.body) {
                    free(n->fn_dec.body->block.ss->syms);
                    free(n->fn_dec.body->block.ss);
                }
        }
    }
    if (!pctx) return 0;
    for (size_t i = 0; i < pctx->ast->arena->pages_count; i++) {
        free(pctx->ast->arena->pages[i]);
    }

    free(pctx->ast->arena->pages);
    free(pctx->ast->arena);
    free(pctx->ast->nodes);
    free(pctx->ast);

    for (size_t i = 0; i < pctx->symbols.syms_count; i++) {
        Symbol s = pctx->symbols.syms[i];
        if (s.sym_type == SymFn) {
            Function f = s.fn;
            /* if (f.args) {
                free(f.args); // TODO maybe use arena instead
            } */
        }
    }
    for (size_t i = 0; i < pctx->symbols.syms_count; i++) {
    }
    free(pctx->symbols.syms);
    for (size_t i = 0; i < pctx->gpa.pages_count; i++) {
        free(pctx->gpa.pages[i]);
    }
    free(pctx->gpa.pages);
    free(pctx);
    return 1;
}


Variable* ss_get_variable(SymbolStore* ss, Name name) {
    for (size_t i = 0; i < ss->syms_count; i++) {
        if (ss->syms[i].sym_type == SymVar) {
            Variable* var = &ss->syms[i].var;
            if (name_cmp(name, var->name)) {
                return var;
            }
        }
    }
    if (ss->parent != NULL) {
        return ss_get_variable(ss->parent, name);
    }
    return NULL;
}
Type* ss_get_type(SymbolStore* ss, Name name) {
    if (ss_sym_exists(ss, name) == SymNone) {
        err("Symbol %.*s doesn't exist in symbol store",
            (int)name.length, name.name);
        for (size_t i = 0; i < ss->syms_count; i++) {
            if (ss->syms[i].sym_type == SymVar) {
                // printf("\tVar : "); print_name(ss->syms[i].var.name);
            }
            if (ss->syms[i].sym_type == SymType) {
                // printf("\tType: "); print_name(ss->syms[i].type.name);
            }
        }
        return NULL;
    }
    for (size_t i = 0; i < ss->syms_count; i++) {
        if (ss->syms[i].sym_type == SymType) {
            Type* t = &ss->syms[i].type;
            if (name_cmp(name, t->name)) {
                return t;
            }
        }
    }
    if (ss->parent != NULL) {
        return ss_get_type(ss->parent, name);
    }
    warn("Doesn't have a parent.");
    return NULL;
}


Node* alloc_node(ParserCtx* pctx) {
    Node n;
    n.token = (Token){TokenEOF, 0};
    n.kind = NodeNone;
    Node* ret_n = arena_add_node(pctx->ast->arena, n);
    return ret_n;
}
Node* new_node(ParserCtx* pctx, NodeKind type, Token token) {

    Node* n = (Node*)arena_alloc(pctx->ast->arena, sizeof(Node));
    if (!n) return n;
    memset(n, 0, sizeof(Node));
    n->token = token;
    n->kind = type;
    return n;
}

SymbolStore* ss_new(SymbolStore* parent) {
    // TODO: find a better way cus malloc is awkward
    SymbolStore* _ss = (SymbolStore*)malloc(sizeof(SymbolStore));
    if (!_ss) {
        err("Failed to allocate symbol store.");
        return 0;
    }
    _ss->syms_capacity = 256;
    _ss->syms = (Symbol*)malloc(_ss->syms_capacity*sizeof(Symbol));
    if (!_ss->syms) {
        err("Failed to allocate memory for symbol store symbols.");
        return 0;
    }
    _ss->syms_count = 0;
    _ss->parent = parent; // set symbol store parent
    return _ss;
}

ParseRes pr_ok(Node* n) {
    return (ParseRes){.ok=PrOk,.node=n};
}
ParseRes pr_ok_many(Node* nodes[10], size_t count) {
    ParseRes pr;
    pr.ok = PrMany;
    pr.many.count = count;
    memcpy(pr.many.nodes, nodes, count*sizeof(Node*));
    return pr;
}
ParseRes pr_fail() {
    return (ParseRes){PrFail,NULL};
}
// returns 1 on true
int is_cmpt_constant(Node* expr) {
    switch (expr->kind) {
        case NodeBinOp: {
            if (!is_cmpt_constant(expr->binop.left)) 
                return 0;
            else if (!is_cmpt_constant(expr->binop.right)) 
                return 0;
        } break;
        case NodeNumLit: {
        } break;
        case NodeUnary: {
            if (!is_cmpt_constant(expr->unary.target)) 
                return 0;
        } break;
        case NodeVarDec:  {
            if (!expr->var_dec.value) break;
            if (!is_cmpt_constant(expr->var_dec.value)) 
                return 0;
        } break;
        default:
            err("can not determinale compile time value of expression:"
                    " %s.", get_token_data(expr->token)); return 0;
    }
    return 1;
}

int ast_add_node(AST* ast, Node* n) {
    ast->nodes[ast->nodes_count++] = n;
    if (ast->nodes_count >= ast->max_nodes) {
        ast->max_nodes *= 2;
        ast->nodes = (Node**)realloc(ast->nodes, ast->max_nodes);
        if (ast->nodes == NULL) {
            err( "Failed to reallocate memory for ast.");
            exit(1);
            return 1;
        }
    }
    return 0;
}

Node* arena_add_node(Arena* a, Node n) {
    a->node_allocations++;
    return (Node*)arena_add(a, sizeof(Node), &n);
}
