#ifndef PARSER_H
#define PARSER_H
#include "constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "constants.h"
#include "lexer.h"
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
    // symbol related shi
    NodeSymbol,
    NodeModuleAccess,
    NodeConstDec,   // both fns and vars
    NodeVarDec,     // both fns and vars
    NodeTypeData,   // symbol stuff ig
    NodeFnDec,
    NodeFnCall, // 8

    // return
    NodeRet,
    // expression shi
    // function/stmt stuff
    NodeFn, // 10
    NodeScope, // 11
    // literals
    NodeStringLit, // 12
    NodeNumLit, // 13
    NodeStructLit, // 14
    // ops
    NodeUnary, // 15
    NodeBinOp, // 16
    // cast
    NodeCast,
    // access
    NodeFieldAccess,
    NodeIndex,
    NodeCount, // counut
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
struct Type {
    long uutid; // universal unice type id for type comparason
    TypeKind kind;
    size_t size;
    Span name;
    Node* symbol; // path if module access, symbol if name. for parsing
    Type* alias;
    union {
        Type* ptr;
    };
};
struct Node {
    Token token;
    Type* type;
    Symbol* st_symbol; // resolved symbol
    NodeKind kind;
    union {
        //symbols stuf like vars and decs
        Span symbol;
        struct {
            Node* symbol;
            Node* type;
            Node* value;
            int is_const;
        } var_dec;
        Variable* var;
        Type* type_data;
        struct {
            Span target;
            Node* module;
            Parser* p;
        } module_access;
        struct {
            Node* target;
            Node** args;
            size_t args_count;
        } fn_call;
        struct {
            Node* symbol;
            Node* fn_body;
        } fn_dec;

        // return
        struct {
            Node* expr;
        } ret;
        // expression styff
        struct {
            FnDecArg* args;
            size_t count;
            Node* return_type;
            Node* body;
        } fn_body;
        struct {
            Node** stmts;
            size_t count;
        } scope;
        // literals
        Span string_literal;
        struct {
            double number;
            Span str_repr;
        } number;
        struct {
            struct {Span name; Node* node;}* fields[10];
            size_t count;
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
    };
};



struct Parser {
    char*           path;
    Span            module_name;
    Span            module_code;
    Lexer*          l;
    size_t          tokens_count, tokens_index;
    Arena           arena;
    Node**          nodes;
    size_t          nodes_count;
    size_t          nodes_cap;
    SymbolTable*    syms;
};
static const size_t ptr_size = PTR_SIZE;
Parser* pctx_new(Lexer* l, char* path, SymbolTable* st);
int parse(Parser *p);
int parser_destry(Parser *p);
Span get_name_from_path(const char *path);
// parser bs
Node* parse_expression(Parser *p);
Node* parse_type(Parser* p);
Node* parse_path(Parser* p);
Node* parse_symbol(Parser* p);
Node* parse_fn_body(Parser* p);
Node* parse_statement(Parser *p);

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
    Arena* arena;
    size_t count, cap;
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
typedef struct {
    Span name;
    Type* type;
} Field;
struct Symbol {
    SymKind kind;
    int is_public;
    Span name; // again
    union {
        Variable var;
        Field field;
        Type type;
    };
};
#define TYPE(t, tsize)  (Type){.kind=tt_##t, .size=tsize\
    , .name=(Span){(char*)#t, sizeof(#t) - 1}},
static Type  base_types[] = {
    TYPE(fn,    ptr_size) // it's a pointer
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
SymbolTable*    st_new(Parser* p, SymbolTable* parent);
int             st_destroy(SymbolTable* st);
Symbol*         st_add_var(SymbolTable* st, Variable v);
Symbol*         st_add_type(SymbolTable* st, Type t);
Symbol*         st_get_var(SymbolTable* st, Span name);
Symbol*         st_get_type(SymbolTable* st, Span name);
Symbol*         st_sym_exists(SymbolTable* st, Span name);
// sets type to known type in st;
Type*           st_resolve_type(SymbolTable* st, Type* t);

long new_uutid();
int add_base_types(SymbolTable* st);
#endif // PARSER_H
