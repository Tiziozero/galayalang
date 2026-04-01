#ifndef PARSER_H
#define PARSER_H
#include "constants.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "constants.h"
#include "lexer.h"
#include "parse_number.h"
#include "utils.h"
#ifndef PTR_SIZE
#define PTR_SIZE sizeof(void*)
#endif

typedef struct ProgramState ProgramState;
typedef struct Node Node;
typedef struct Variable Variable;
typedef struct Type Type;
typedef struct Parser Parser;
typedef struct SymbolTable SymbolTable;
typedef struct Symbol Symbol;
typedef enum {
    NodeEmpty=1, // empty node. no info
    NodeSymbol, // symbol related shi
    NodeModuleAccess,
    NodeConstDec,   // both fns and vars
    NodeVarDec,     // both fns and vars
    NodeTypeData,   // symbol stuff ig
    NodeFnDec,
    NodeFnLit,
    NodeFnCall,
    NodeArg, // fn dec arg
    NodeNodeList, // list of nodes for whatever
    NodeRet,
    NodeBlock, // 11 // fn
    NodeStringLit, // 12
    NodeNumLit, // 13
    NodeStructLit, // 14
    NodeUnary, // 15
    NodeBinOp, // 16
    NodeCast,
    NodeFieldAccess,
    NodeIndex,
    NodeCount, // counut
    NodeIfStmt,
    NodeStructDec,
    NodeFieldDec, // field dec
    NodeNamedField, // "x: 32*a" or sm shit
    NodeNone=0,
} NodeKind;
typedef struct {
    Span arg;
    Node* type;
} FnDecArg;
typedef enum {
    tt_to_determinate = 0,
    tt_fn,
    tt_u8,
    tt_u16,
    tt_u32,
    tt_u64,
    tt_u128,
    tt_i8,
    tt_i16,
    tt_i32,
    tt_i64,
    tt_i128,
    tt_f32,
    tt_f64,
    tt_ptr,
    tt_usize,
    tt_char,
    tt_struct,
    tt_untyped_unsigned_int,
    tt_untyped_int, // could be either ig
    tt_untyped_float,
    tt_untyped_struct,
    tt_void,
} TypeKind;
typedef struct {
    Node* args;
    Type* return_type;
} FunctionType;
typedef struct {
    Span name;
    Type* type;
} Field;
typedef struct {
    Span name;
    Symbol** fields; // change to Symbol** for resolving fields in struct_lit
    int count;
} StructType;
struct Type {
    long uutid; // universal unice type id for type comparason
    TypeKind kind;
    int size;
    Span name;
    Node* ident; // path if module access, symbol if name. for parsing
    Type* alias;
    union {
        Type* ptr;
        FunctionType fn;
        StructType struct_t;
    };
    int resolved, unfinished; // flags
};
struct Node {
    int resolved, yields_value;
    Token token;
    Type* type;
    Symbol* symbol; // resolved symbol
    NodeKind kind;
    union {
        //symbols stuf like vars and decs
        Span ident;
        struct {
            int a, b;
            Node* ident; // idk, don't change to Span as Typecheck might
                         // need to set type or not idk yet
            Node* type;
            Node* value;
            int is_const;
        } var_dec;
        Type* type_data;
        struct {
            Span target;
            Node* module;
            Parser* p;
        } module_access;
        struct {
            Node* target;
            Node* args;
            int args_count;
        } fn_call;
        struct {
            Node* ident;
            Node* type;
        } arg; // arg
        struct {
            Node** nodes;
            int count;
        } node_list; // list of nodes for calling fn args or whatever else
        struct {
            Node* ident; // null if lambda
            Node* args; // NodeList
            Node* return_type;
            Node* body; // statement
        } fn_dec;
        // return
        struct {
            Node* expr;
        } ret;
        // expression styff
        struct {
            Node** stmts;
            int count;
            Node* last;
        } block;
        struct {
            Node* cond, *block, *else_block;
            Node** alt_conds, **alt_blocks;
            int alt_count;
        } if_stmt;
        // literals
        Span string_literal;
        struct {
            double number; // float
            uint64_t integer;
            NumKind kind;
            Span str_repr;
        } number;
        struct {
            Node* type_name; // should resolve to type
            Node* fields; // NodeList
        } struct_literal;
        // ops
        struct {
            UnaryType type;
            Node* target;
        } unary;
        struct {
            OpType type;
            Node* left;
            Node* right;
        } binop;
        // cast
        struct {
            Type* to;
            Node* target;
        } cast;
        // access
        struct {
            Node* target, *index;
        } index;
        struct {
            Node* target;
            Span field_name;
        } field_access;
        struct {
            Node* ident; // symbol
            Node* type;
        } field_dec;
        struct {
            Node* ident; // symbol
            Node* expr;
        } named_field;
        struct {
            Node* field_decs; // NodeList
            Node* ident;
        } struct_dec;
    };
};



struct Parser {
    char*           path;
    Span            module_name;
    Span            module_code;
    Lexer*          l;
    int          tokens_count, tokens_index;
    Arena           arena;
    Node**          nodes;
    int          nodes_count;
    int          nodes_cap;
    SymbolTable*    syms;
};
static const int ptr_size = PTR_SIZE;
Parser* pctx_new(Lexer* l, char* path, SymbolTable* st);
int parse(Parser *p);
int parser_destry(Parser *p);
Span get_name_from_path(const char *path);
// parser bs
Node* parse_expression(Parser *p);
Node* parse_type(Parser* p);
Node* parse_path(Parser* p);
Node* parse_symbol(Parser* p);
Node* parse_fn_dec(Parser *p);
Node* parse_statement(Parser *p);
Node* parse_if_else(Parser* p);
Node* parse_condition(Parser*p);
Node* parse_field_decs(Parser* p);
Node* parse_arg_decs(Parser* p);
Node* parse_scope(Parser *p);

#define expect(p, t) \
do { \
    if (current(p).type != (t)) { \
        panic("Expected %s, got %s.", #t, get_token_data(current(p))); \
        return NULL; \
    } \
} while (0)

#define expect_kw(p, k) \
do { \
    if (current(p).type != TokenKeyword || current(p).kw != (k)) { \
        panic("Expected keyword %s, got %s.", #k, get_token_data(current(p))); \
        return NULL; \
    } \
} while (0)
Token current(Parser* p);
Token peek(Parser* p);
Token consume(Parser* p);

Node* new_node(Parser* p);
Type* new_type(Parser* p);
int is_valid_type(Type* t);
// symbol check
struct SymbolTable {
    SymbolTable* parent;
    Symbol** symbols;
    int count, cap;
    Type** types;
    int types_count, types_cap;
    Arena* arena;
    Parser* p;
};
typedef enum {
    SymVar = 1,
    SymType,
    SymField,
    SymArg,
    SymCount, // count
    SymNone = 0, // 0
} SymKind;
struct Variable {
    Span name;
    Type* type;
};
typedef Variable Argument;
struct Symbol {
    SymKind kind;
    int is_public;
    Span name; // again
    union {
        Variable var;
        Argument arg;
        Field field;
        Type type;
    };
};
#define TYPE(t, tsize)  (Type){.kind=tt_##t, .size=tsize\
    , .name=(Span){(char*)#t, sizeof(#t) - 1}},
static Type  base_types[] = {
    TYPE(u8,    1)
    TYPE(u16,   2)
    TYPE(u32,   4)
    TYPE(u64,   8)
    TYPE(u128,  16)
    TYPE(i8,    1)
    TYPE(i16,   2)
    TYPE(i32,   4)
    TYPE(i64,   8)
    TYPE(i128,  16)
    TYPE(f32,   4)
    TYPE(f64,   8)
    // TYPE(ptr,   ptr_size)
    TYPE(usize, ptr_size)
    TYPE(char, 1)
    TYPE(void,  0)
    // TYPE(none,  0)
};
#undef TYPE
int resolve_symbols(Parser* p);
int symbols(Parser* p, SymbolTable*s, Node* n);
SymbolTable*    st_new(Parser* p, SymbolTable* parent);
long new_uutid();
int add_base_types(SymbolTable* st);
Symbol*         st_add_var(SymbolTable* st, Variable v);
Symbol*         st_add_arg(SymbolTable* st, Argument a);

Symbol*         st_add_type(SymbolTable* st, Type t);
Symbol*         st_add_unfinished_type(SymbolTable* st, Type t);
Symbol*         complete_type(Symbol* unfinished, Type t);

Symbol*         st_sym_exists(SymbolTable* st, Span name);
Symbol* st_sym_exists_scope(SymbolTable* st, Span name); // vars, whatever can
                                                         // be overshadowed
Symbol*         st_get_var(SymbolTable* st, Span name);
Symbol*         st_get_object(SymbolTable* st, Span name); // var, arg, anything that holds a value
Symbol*         st_get_type(SymbolTable* st, Span name);
// sets type to known type in st;
Type*           st_resolve_type(SymbolTable* st, Type* t);

int             st_destroy(SymbolTable* st);

int type_check(Parser* p);
int is_untyped(Type* t);

int is_untyped(Type* t);

int is_float(Type* t);

int is_unsigned(Type* t);

int is_signed(Type* t);

int is_integer(Type* t);

int is_numeric(Type* t);

int is_pointer(Type* t);

int is_struct(Type* t);

int is_void(Type* t);

int is_fn(Type* t);

/* Returns 1 if the type can be used in arithmetic expressions */
int is_arithmetic(Type* t);

/* Returns 1 if the type can be compared with < > <= >= */
int is_ordered(Type* t);

/* Returns 1 if the type can be compared with == != */
int is_comparable(Type* t);

/* Returns 1 if the type has a known size at compile time */
int is_sized(Type* t);

/* Returns 1 if the type needs to be resolved/inferred still */
int is_undetermined(Type* t);

/* Returns 1 if the type has a known size at compile time */
int is_sized(Type* t);

/* Returns 1 if the type needs to be resolved/inferred still */
int is_undetermined(Type* t);
int can_binop(Type* t);
int is_lvalue(Node* lvalue); // for reference/assignment etc
static inline const char* NodeKindToString(NodeKind kind) {
    switch (kind) {
        case NodeEmpty: return "NodeEmpty";
        case NodeSymbol: return "NodeSymbol";
        case NodeModuleAccess: return "NodeModuleAccess";
        case NodeConstDec: return "NodeConstDec";
        case NodeVarDec: return "NodeVarDec";
        case NodeTypeData: return "NodeTypeData";
        case NodeFnDec: return "NodeFnDec";
        case NodeFnCall: return "NodeFnCall";
        case NodeArg: return "NodeArg";
        case NodeNodeList: return "NodeNodeList";
        case NodeRet: return "NodeRet";
        case NodeBlock: return "NodeBlock";
        case NodeStringLit: return "NodeStringLit";
        case NodeNumLit: return "NodeNumLit";
        case NodeStructLit: return "NodeStructLit";
        case NodeUnary: return "NodeUnary";
        case NodeBinOp: return "NodeBinOp";
        case NodeCast: return "NodeCast";
        case NodeFieldAccess: return "NodeFieldAccess";
        case NodeIndex: return "NodeIndex";
        case NodeCount: return "NodeCount";
        case NodeIfStmt: return "NodeIfStmt";
        case NodeStructDec: return "NodeStructDec";
        case NodeFieldDec: return "NodeFieldDec";
        case NodeNamedField: return "NodeNamedField";
        case NodeFnLit: return "NodeFnLit";
        case NodeNone: return "NodeNone";
        default: panic("Implement %d", kind);
    }
}

// check if all good
int type_is_in_st(SymbolTable* st, Type* t);
int check_type(Parser* p, SymbolTable* st, Type* t, Token tok, Node* n);
int node_all_good(Parser* p, SymbolTable* st, Node* n);
int all_good(Parser* p);

static inline void print_type(Type* t) {
    if (!t) {
        printf("<null type>");
        return;
    }
    if (is_undetermined(t)) {
        printf("<unresolved kind=%d>", t->kind);
        return;
    }
    if (is_untyped(t)) {
        printf("<untyped kind=%d size=%d>", t->kind, t->size);
        return;
    }
    if (t->kind == tt_ptr) {
        printf("Type { ptr to ");
        print_type(t->ptr);
        printf(" size=%d }", t->size);
        return;
    }
    if (t->kind == tt_fn) {
        printf("Type{ fn ret=");
        print_type(t->fn.return_type);
        printf(" }");
        return;
    }
    printf("Type{ name=%.*s size=%d kind=%d }",
        (int)t->name.length, t->name.name,
        t->size, t->kind);
}
static inline void print_node_to_file(FILE* f, Node* n, int indent);

static inline void print_indent(FILE* f, int indent) {
    for (int i = 0; i < indent; i++) fprintf(f, "  ");
}

static inline void print_type_to_file(FILE* f, Type* t) {
    if (!t) { fprintf(f, "<null type>"); return; }
    if (t->kind == tt_ptr) {
        fprintf(f, "*");
        print_type_to_file(f, t->ptr);
        return;
    }
    if (is_undetermined(t)) { fprintf(f, "<unresolved kind=%d>", t->kind); return; }
    if (is_untyped(t))      { fprintf(f, "<untyped kind=%d size=%d>", t->kind, t->size); return; }
    fprintf(f, "%.*s", (int)t->name.length, t->name.name);
}

static inline void print_node_to_file(FILE* f, Node* n, int indent) {
    if (!n) { print_indent(f, indent); fprintf(f, "<null node>\n"); return; }
    print_indent(f, indent);
    fprintf(f, "[%s]", NodeKindToString(n->kind));
    if (n->type) { fprintf(f, " : "); print_type_to_file(f, n->type); }
    fprintf(f, "\n");
    switch (n->kind) {
        case NodeSymbol:
            print_indent(f, indent+1);
            fprintf(f, "ident: %.*s\n", (int)n->ident.length, n->ident.name);
            break;
        case NodeVarDec:
        case NodeConstDec:
            print_indent(f, indent+1);
            fprintf(f, "ident:\n");
            print_node_to_file(f, n->var_dec.ident, indent+2);
            if (n->var_dec.type) {
                print_indent(f, indent+1);
                fprintf(f, "type:\n");
                print_node_to_file(f, n->var_dec.type, indent+2);
            }
            if (n->var_dec.value) {
                print_indent(f, indent+1);
                fprintf(f, "value:\n");
                print_node_to_file(f, n->var_dec.value, indent+2);
            }
            break;
        case NodeFnDec:
            print_indent(f, indent+1);
            fprintf(f, "ident:\n");
            printf("printing fn ident (%d)",
                    n->fn_dec.ident->kind);
            fflush(f);
            print_node_to_file(f, n->fn_dec.ident, indent+2);
            fflush(f);
            printf("printing fn ident f");
            fflush(f);
            if (n->fn_dec.args) {
                print_indent(f, indent+1);
                fprintf(f, "args:\n");
                print_node_to_file(f, n->fn_dec.args, indent+2);
            }
            if (n->fn_dec.return_type) {
                print_indent(f, indent+1);
                fprintf(f, "return_type:\n");
                print_node_to_file(f, n->fn_dec.return_type, indent+2);
            }
            if (n->fn_dec.body) {
                print_indent(f, indent+1);
                fprintf(f, "body:\n");
                print_node_to_file(f, n->fn_dec.body, indent+2);
            }
            break;
        case NodeFnCall:
            print_indent(f, indent+1);
            fprintf(f, "target:\n");
            print_node_to_file(f, n->fn_call.target, indent+2);
            if (n->fn_call.args) {
                print_indent(f, indent+1);
                fprintf(f, "args:\n");
                print_node_to_file(f, n->fn_call.args, indent+2);
            }
            break;
        case NodeArg:
            print_indent(f, indent+1);
            fprintf(f, "ident:\n");
            print_node_to_file(f, n->arg.ident, indent+2);
            if (n->arg.type) {
                print_indent(f, indent+1);
                fprintf(f, "type:\n");
                print_node_to_file(f, n->arg.type, indent+2);
            }
            break;
        case NodeNodeList:
            print_indent(f, indent+1);
            fprintf(f, "count: %d\n", n->node_list.count);
            for (int i = 0; i < n->node_list.count; i++)
                print_node_to_file(f, n->node_list.nodes[i], indent+1);
            break;
        case NodeBlock:
            print_indent(f, indent+1);
            fprintf(f, "stmts: %d\n", n->block.count);
            for (int i = 0; i < n->block.count; i++)
                print_node_to_file(f, n->block.stmts[i], indent+1);
            break;
        case NodeRet:
            if (n->ret.expr) {
                print_indent(f, indent+1);
                fprintf(f, "expr:\n");
                print_node_to_file(f, n->ret.expr, indent+2);
            }
            break;
        case NodeIfStmt:
            print_indent(f, indent+1);
            fprintf(f, "cond:\n");
            print_node_to_file(f, n->if_stmt.cond, indent+2);
            print_indent(f, indent+1);
            fprintf(f, "block:\n");
            print_node_to_file(f, n->if_stmt.block, indent+2);
            for (int i = 0; i < n->if_stmt.alt_count; i++) {
                print_indent(f, indent+1);
                fprintf(f, "else if cond[%d]:\n", i);
                print_node_to_file(f, n->if_stmt.alt_conds[i], indent+2);
                print_indent(f, indent+1);
                fprintf(f, "else if block[%d]:\n", i);
                print_node_to_file(f, n->if_stmt.alt_blocks[i], indent+2);
            }
            if (n->if_stmt.else_block) {
                print_indent(f, indent+1);
                fprintf(f, "else:\n");
                print_node_to_file(f, n->if_stmt.else_block, indent+2);
            }
            break;
        case NodeBinOp:
            print_indent(f, indent+1);
            fprintf(f, "op: %d\n", n->binop.type);
            print_node_to_file(f, n->binop.left,  indent+1);
            print_node_to_file(f, n->binop.right, indent+1);
            break;
        case NodeUnary:
            print_indent(f, indent+1);
            fprintf(f, "op: %d\n", n->unary.type);
            print_node_to_file(f, n->unary.target, indent+1);
            break;
        case NodeNumLit:
            print_indent(f, indent+1);
            fprintf(f, "value: %.*s\n",
                (int)n->number.str_repr.length, n->number.str_repr.name);
            break;
        case NodeStringLit:
            print_indent(f, indent+1);
            fprintf(f, "value: \"%.*s\"\n",
                (int)n->string_literal.length, n->string_literal.name);
            break;
        case NodeModuleAccess:
            print_indent(f, indent+1);
            fprintf(f, "target: %.*s\n",
                (int)n->module_access.target.length, n->module_access.target.name);
            print_node_to_file(f, n->module_access.module, indent+1);
            break;
        case NodeFieldAccess:
            print_indent(f, indent+1);
            fprintf(f, "field: %.*s\n",
                (int)n->field_access.field_name.length,
                n->field_access.field_name.name);
            print_node_to_file(f, n->field_access.target, indent+1);
            break;
        case NodeIndex:
            print_indent(f, indent+1);
            fprintf(f, "target:\n");
            print_node_to_file(f, n->index.target, indent+2);
            print_indent(f, indent+1);
            fprintf(f, "index:\n");
            print_node_to_file(f, n->index.index, indent+2);
            break;
        case NodeCast:
            print_indent(f, indent+1);
            fprintf(f, "to: "); print_type_to_file(f, n->cast.to); fprintf(f, "\n");
            print_indent(f, indent+1);
            fprintf(f, "target:\n");
            print_node_to_file(f, n->cast.target, indent+2);
            break;
        case NodeTypeData:
            print_indent(f, indent+1);
            fprintf(f, "type: "); print_type_to_file(f, n->type_data); fprintf(f, "\n");
            break;
        case NodeStructDec:
            print_indent(f, indent+1);
            fprintf(f, "ident:\n");
            print_node_to_file(f, n->struct_dec.ident, indent+2);
            if (n->struct_dec.field_decs) {
                print_indent(f, indent+1);
                fprintf(f, "fields:\n");
                print_node_to_file(f, n->struct_dec.field_decs, indent+2);
            }
            break;
        case NodeFieldDec:
            print_indent(f, indent+1);
            fprintf(f, "ident:\n");
            print_node_to_file(f, n->field_dec.ident, indent+2);
            if (n->field_dec.type) {
                print_indent(f, indent+1);
                fprintf(f, "type:\n");
                print_node_to_file(f, n->field_dec.type, indent+2);
            }
            break;
        case NodeStructLit:
            print_indent(f, indent+1);
            fprintf(f, "type_name:\n");
            print_node_to_file(f, n->struct_literal.type_name, indent+2);
            if (n->struct_literal.fields) {
                print_indent(f, indent+1);
                fprintf(f, "fields:\n");
                print_node_to_file(f, n->struct_literal.fields, indent+2);
            }
            break;
        case NodeNamedField:
            print_indent(f, indent+1);
            fprintf(f, "ident:\n");
            print_node_to_file(f, n->named_field.ident, indent+2);
            print_indent(f, indent+1);
            fprintf(f, "expr:\n");
            print_node_to_file(f, n->named_field.expr, indent+2);
            break;
        case NodeEmpty:
        case NodeNone:
            break;
        case NodeCount:
            print_indent(f, indent+1);
            fprintf(f, "<NodeCount — bug>\n");
            break;
    }
}

static inline void print_parser_to_file(FILE* f, Parser* p) {
    fprintf(f, "=== Parser: %.*s ===\n",
        (int)p->module_name.length, p->module_name.name);
    fprintf(f, "nodes: %d\n\n", p->nodes_count);
    for (int i = 0; i < p->nodes_count; i++) {
        fprintf(f, "--- tls[%d] ---\n", i);
        print_node_to_file(f, p->nodes[i], 0);
    }
}
static inline void print_st(SymbolTable* st, int depth) {
    if (!st) return;

    char indent[128] = {0};
    for (int i = 0; i < depth * 2 && i < 127; i++) indent[i] = ' ';

    printf("%s[SymbolTable @ %p] (parent: %p)\n", indent, (void*)st, (void*)st->parent);

    printf("%s  symbols (%d/%d):\n", indent, st->count, st->cap);
    for (int i = 0; i < st->count; i++) {
        Symbol* sym = st->symbols[i];
        if (!sym) { printf("%s    [%d] NULL\n", indent, i); continue; }

        const char* kind_str;
        switch (sym->kind) {
            case SymVar:   kind_str = "var";   break;
            case SymType:  kind_str = "type";  break;
            case SymField: kind_str = "field"; break;
            case SymArg:   kind_str = "arg";   break;
            default:       kind_str = "none";  break;
        }

        printf("%s    [%d] (%s) %.*s", indent, i, kind_str,
            (int)sym->name.length, sym->name.name);

        // print the type depending on kind
        Type* t = NULL;
        switch (sym->kind) {
            case SymVar:   t = sym->var.type;   break;
            case SymArg:   t = sym->arg.type;   break;
            case SymField: t = sym->field.type; break;
            case SymType:  t = &sym->type;      break;
            default: break;
        }
        if (t) { printf(" : "); print_type(t); }
        printf("\n");
    }

    printf("%s  types (%d/%d):\n", indent, st->types_count, st->types_cap);
    for (int i = 0; i < st->types_count; i++) {
        Type* t = st->types[i];
        if (!t) { printf("%s    [%d] NULL\n", indent, i); continue; }
        printf("%s    [%d] ", indent, i);
        print_type(t);
        printf("\n");
    }

    if (st->parent) {
        printf("%s  -> parent:\n", indent);
        print_st(st->parent, depth + 1);
    }
}
void type_registry_add(Type* t);
int  type_registry_contains(Type* t);

Node* make_node_list(Parser* p, Node** nodes, int count);
#endif // PARSER_H
