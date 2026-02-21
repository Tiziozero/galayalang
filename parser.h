#ifndef PARSER_H
#define PARSER_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "lexer.h"
#include "utils.h"

typedef struct ParserCtx ParserCtx;
typedef struct ProgramState ProgramState;
typedef struct SymbolStore SymbolStore;
typedef enum {
    NodeNone = 0,
    NodeCast,   // 1
    NodeVarDec, // 2
    NodeVar,    // 3
    NodeFieldAccess,  // 4
    NodeIndex,  // 5
    NodeUnary,  // 6
    NodeNumLit, // 7
    NodeStringLit, // 8
    NodeBinOp,  // 9
    NodeFnDec,  // 10
    NodeIfElse, // 11
    NodeFnCall,
    NodeConditional,
    NodeBlock,
    NodeRet,
    NodeStructDec, // declare struct
    NodeModuleAccess,
    NodeDecModule, // "use"
    NodeUntypedStruct, // untyped struct
    NodeTypeData, // type node
    NodePrintString,
} NodeKind;


typedef enum {
    UnNone = 0,
    UnRef,
    UnDeref,
    UnNegative,
    UnCompliment, // "~": 0b1010 -> 0b0101
    UnNot,
} UnaryType;
typedef enum {
    OpNone = 0,

    // assignment (right-associative)
    OpAssign,

    // logical
    OpOrOr,     // ||
    OpAndAnd,   // &&

    // bitwise
    OpOr,       // |
    OpXor,      // ^
    OpAnd,      // &

    // equality
    OpEq,       // ==
    OpNeq,      // !=

    // relational
    OpLt,       // <
    OpGt,       // >
    OpLe,       // <=
    OpGe,       // >=

    // shifts
    OpLSh,      // <<
    OpRSh,      // >>

    // additive
    OpAdd,      // +
    OpSub,      // -

    // multiplicative
    OpMlt,      // *
    OpDiv,      // /
    OpMod,      // %
    OpComma,    // ,
} OpType;

static const size_t ptr_size = sizeof(void*);


// types
typedef enum {
    tt_to_determinate = 0,
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

const static TypeKind tt_none = tt_to_determinate;
const static TypeKind tt_array = tt_ptr;
typedef struct Type Type;
typedef struct Node Node;
typedef struct Symbol Symbol;
typedef struct Field Field;

typedef struct {Name name; Node* expr;} name_node;

struct Type {
    TypeKind kind;
    int state; // 1 is resolved, 0 is failed
    size_t size;
    Name name;
    Type* alias; // if it's an alias it will have this, otherwise NULL
    union {
        Type* ptr;
        struct {
            Node* size; // element counts
            Type* type;
        } static_array;
        struct {
            Field* fields;
            size_t fields_count;
        } struct_data;
        // add structs here
    };
};

typedef struct {
    Name name;
    Type* type;
    int is_mutable;
} Variable;
typedef struct {
    Name name;
    Type* type;
    int is_mutable;
} Argument;
struct Field {
    Name name;
    Type* type;
    int is_mutable;
};
typedef struct {
    Name name;
    Argument* args;
    size_t args_count;
    size_t args_capacity;
    Type* return_type;
    Node* body; // body to see if it has a declaration
} Function;

typedef struct {
    Name name;
    ParserCtx* pctx;
} Module;

typedef enum {
    SymNone = 0, // fail
    SymVar,
    SymFn,
    SymArg,
    SymModule,
    SymType,
    SymField,
} SymbolType;

struct Symbol {
    SymbolType sym_type;
    union {
        Function    fn;
        Variable    var;
        Type        type;
        Argument    argument;
        Field       field;
        Module      module;
    };
};

struct Node {
    NodeKind kind;
    Token token;
    Type* type;
    Symbol symbol;
    union {
        Variable var;
        Function fn_dec;
        Type type_data;
        Argument arg;
        struct {
            Type* to;
            Node* expr;
        } cast;
        struct {
            Name name;
            Type* type;
            Node* value;
        } var_dec;
        struct {
            Type* type;
            double number;
            Name str_repr;
        } number;
        struct {
            UnaryType type;
            Node* target;
        } unary;
        struct {
            OpType type;
            Node* left;
            Node* right;
        } binop;
        struct {
            Node* base_condition;
            Node* base_block;
            Node** alternate_conditions; // more if blocks
            Node** alternate_blocks;
            size_t count;
            Node* else_block;
        } if_else_con;
        struct {
            Name fn_name;
            Node** args;
            size_t args_count;
        } fn_call;
        struct {
            Node** nodes;
            size_t nodes_count;
            SymbolStore* ss;
        } block;
        struct {
            Node* condition;
            Node* left_true; // if true
            Node* right_false; // if false
        } conditional;
        struct {
            Node* target;
            Node* index_expression;
        } index;
        Node* ret; // return expression
        struct {
            Name name;
            Type* type;
            Node* target;
        } field_access;
        struct {
            Name name;
            size_t fields_count;
            Field* fields; // array of nodes of type field
        } struct_dec;
        struct {
            name_node fields[10];
            size_t count;
        } untyped_strcut;
        Name string_literal;
        struct {
            Node* module;
            Name target;
        } module_access;
        struct {
            Name path;
            ParserCtx* pctx;
            int has_alt_name;
            Name alt_name;
        } module_dec;
        struct {
            Name string;
        } print_string; // cmptime debug stuff
    };
} ;

typedef struct {
    Node** nodes; // array of nodes
    size_t nodes_count;
    size_t max_nodes;
    Arena* arena;
} AST;

#define TYPE(t, tsize)  (Type){.kind=tt_##t, .size=tsize\
    , .name=(Name){(char*)#t, sizeof(#t) - 1}},
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
// 

typedef enum {
    PrFail = 0,
    PrOk, // one node
    PrMany, // many nodes
} ParseResType;
typedef struct {
    ParseResType ok;
    union {
        struct {
            Node* nodes[10];
            size_t count;
        } many;
        Node* node;
    };
} ParseRes;
struct SymbolStore {
    Symbol* syms;
    size_t syms_count;
    size_t syms_capacity;
    SymbolStore* parent;
};
struct ParserCtx {
    Name module_name;
    char* path;
    AST* ast;
    SymbolStore symbols;
    Arena gpa; // general purpose arena
    Token* tokens;
    Lexer* lexer;
    size_t tokens_count;
    size_t tokens_index;
    char* source_code;
    ProgramState* ps;
    SymbolStore** stored_symbol_stores;
    size_t ss_count;;
    size_t ss_cap;;
};

typedef  struct TypeChecker TypeChecker;

struct TypeChecker {
    struct TypeChecker* parent;
    ParserCtx* pctx;
    SymbolStore* ss;
    Function* fn; // for return type
    int ok;
};

static const int err_type_already_exists   = 1;
static const int err_failed_realloc        = 2;
ParserCtx* handle_new_file(ProgramState* ps, char* path);
// parser functions
int             parse(ParserCtx* pctx);

int             check_node_symbol(ParserCtx* pctx, SymbolStore* ss, Node* node);
int             symbols_check(Node* node);
int             type_check_node(TypeChecker* tc, Node* node);

ParseRes        pr_ok(Node* n);
ParseRes        pr_ok_many(Node* nodes[10], size_t count);
ParseRes        pr_fail();
// allocate nodes
Node*           alloc_node(ParserCtx* pctx);
Node*           new_node(ParserCtx* pctx, NodeKind type, Token token);
int             ast_add_node(AST* ast, Node* n);
Node*           arena_add_node(Arena* a, Node n);


// parser tokens
Token           current(ParserCtx* pctx);
Token           peek(ParserCtx* pctx);
Token           consume(ParserCtx* pctx);

// pctx fucntions
// returns 1 on success
ParserCtx*      pctx_new(char* code, Token* tokens, size_t tokens_count,
                    Lexer* lexer, char* path, Name name);
int             pctx_destry(ParserCtx* pctx);

// node stuff
int             is_cmpt_constant(Node* expr);


// symbolstore
SymbolStore*    ss_new(ParserCtx* pctx, SymbolStore* parent);
Function*       ss_get_fn(SymbolStore* ss, Name name);
Variable*       ss_get_variable(SymbolStore* ss, Name name);
Type*           ss_get_type(SymbolStore* ss, Name name);

int             ss_new_var(SymbolStore* ss, Variable var);
int             ss_new_type(SymbolStore* ss, Type t);
int             ss_new_fn(SymbolStore* ss, Function fn);
int             ss_new_field(SymbolStore* ss, Field f);
int             ss_new_module(SymbolStore* ss, Module m);

// returns type on found, 0 on none
SymbolType      ss_sym_exists(SymbolStore* ss, Name name);
// for variables
// returns type on found, 0 on none
SymbolType      ss_sym_exists_scope(SymbolStore* ss, Name name);
// type helper
/*
 * get type from symbol store to type.
 */
struct TypeChecker* new_tc(
                struct TypeChecker* tc, ParserCtx* pctx, SymbolStore* ss);
int             determinate_type(SymbolStore* ss, Type* _type);
/*
 * determinates the lowest level type
 * like if it's a *u32 it returns u32
 * unwraps type, essentially.
 */

int             type_check_expression(TypeChecker* tc, Node *node);
Type*           get_lowest_type(Type* _t);
/* =========================
   Type-level checks
   ========================= */

int type_is_untyped(Type* t);
int type_is_unsigned(Type* t);
int type_is_signed(Type* t);
int type_is_float(Type* t);
int type_is_ptr(Type* t);
int type_is_struct(Type* t);
int type_is_numeric(Type* t);

/* =========================
   TypeState (untyped) checks
   ========================= */

/* =========================
   NodeTypeInfo checks
   ========================= */


/* =========================
   Node-level helpers
   ========================= */

int node_is_untyped(Node* n);

int node_is_numeric(Node* n);
int node_can_binop(Node* n);

void            _cmptime_log_caller(const char *fmt, ...);


Name get_name_from_path(const char *path);
// error functions?
void            err_sym_exists(Name name);
// get sym type
// returns 1 on true

#endif // PARSER_H
