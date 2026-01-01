#ifndef PARSER_H
#define PARSER_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "lexer.h"
#include "utils.h"
typedef struct SymbolStore SymbolStore;
typedef enum {
    NodeNone = 0,

    NodeCast,
    NodeVarDec,
    NodeVar,
    NodeField,
    NodeIndex,
    NodeUnary,
    NodeNumLit,
    NodeArg,
    // NodeAssignment,
    NodeBinOp,
    NodeFnDec,
    NodeIfElse,
    NodeFnCall,
    NodeConditional,
    NodeBlock,
    NodeRet,
    NodeTypeData, // type node
    NodePrintString,
} NodeType;


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

typedef enum {
    none_t = 0,
    struct_t,
    enum_t,
    union_t,
} aggregate_types;

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
    tt_char,
    tt_ptr,
    tt_array,
    tt_struct,
    tt_enum,
    tt_union,
    tt_void,
    tt_fn,
    tt_none,
} TypeType;

typedef struct Type Type;
typedef struct Node Node;
struct Type {
    TypeType type;
    size_t size;
    Name name;
    union {
        Type* ptr;
        struct {
            Node* size;
            Type* type;
        } static_array;
        struct {
            size_t size;
            Name name;
            // fields etc
        } struct_data;
        // add structs here
    };
};

struct Node {
    NodeType type;
    Type expr_type;
    Token token;
    union {
        struct{
            Name name;
            Node* type;
        } arg;
        struct {
            Node* to;
            Node* expr;
        } cast; // change once type are implemented?
        struct {
            Name name;
            Type type;
            Node* value;
        } var_dec; // change once type are implemented?
        struct {
            Name name;
        } var; // change once type are implemented?
        struct {
            Type type;
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
            Name name;
            Node* body;
            Node* return_type;
            // type
            Node** args;
            size_t args_count;
            // add type and args
        } fn_dec;
        struct {
			Node* base_condition;
			Node* base_block;
            Node** alternate_conditions; // more if blocks
            Node** alternate_blocks;
            size_t count;
            Node* else_block;
        } if_else_con;
        struct {
            Name fn_name; // name will be a node/expression
            Node** args;
            size_t args_count;
            // add type and args
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
			Node* term;
			Node* index_expression;
		} index;
        Type type_data;
        Node* ret; // expression
        struct {
            Name string;
        } print_string;
    };
} ;

typedef struct {
    Node** nodes; // array of nodes
    size_t nodes_count;
    size_t max_nodes;
    Arena* arena;
} AST;

static Type  known_types[] = {
    (Type){.type=tt_char, .size=1, .name=(Name){"char", 4}},
    (Type){.type=tt_i8, .size=1, .name=(Name){"i8", 2}},
    (Type){.type=tt_i16, .size=2, .name=(Name){"i16", 3}},
    (Type){.type=tt_i32, .size=4, .name=(Name){"i32", 3}},
    (Type){.type=tt_i64, .size=8, .name=(Name){"i64", 3}},
    (Type){.type=tt_i128,.size=16, .name=(Name){"i128", 4}},
                       
    (Type){.type=tt_u8, .size=1, .name=(Name){"u8", 2}},
    (Type){.type=tt_u16, .size=2, .name=(Name){"u16", 3}},
    (Type){.type=tt_u32, .size=4, .name=(Name){"u32", 3}},
    (Type){.type=tt_u64, .size=8, .name=(Name){"u64", 3}},
    (Type){.type=tt_u128, .size=16, .name=(Name){"u128", 4}},
    (Type){.type=tt_f32, .size=4, .name=(Name){"f32", 3}},
    (Type){.type=tt_f64, .size=8, .name=(Name){"f64", 3}},
    (Type){.type=tt_ptr, .size=ptr_size, .name=(Name){"ptr", 3}},
    (Type){.type=tt_void, .size=0, .name=(Name){"void", 4}},
};
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


// symbol store
typedef struct Symbol Symbol;
typedef struct {
    Name name;
    Type type;
} Variable;
typedef struct {
    Name name;
    Type type;
} Argument;
typedef struct {
    Name name;
    Type type;
} Field;
typedef struct {
    Name name;
    Argument* args;
    size_t args_count;
    size_t args_capacity;
    Type return_type;
    SymbolStore* ss;
} Function;
typedef enum {
    SymNone = 0, // fail
    SymVar,
    SymFn,
    SymArg,
    SymType,
    SymField,
} SymbolType;

struct Symbol {
    SymbolType sym_type;
    union {
        Function fn;
        Variable var;
        Type type;
        Argument argument;
        Field field;
    };
};
struct SymbolStore {
    Symbol* syms;
    size_t syms_count;
    size_t syms_capacity;
    SymbolStore* parent;
};
typedef struct {
    AST* ast;
    SymbolStore symbols;
    Arena gpa; // general purpose arena
    Token* tokens;
    size_t tokens_count;
    size_t tokens_index;
} ParserCtx;


static const int err_type_already_exists   = 1;
static const int err_failed_realloc        = 2;
// parser functions
ParserCtx*      parse(Lexer* l);

int             type_check_node(SymbolStore* ss, Node* node);
int             check_node_symbol(SymbolStore* ss, Node* node);

ParseRes        pr_ok(Node* n);
ParseRes        pr_ok_many(Node* nodes[10], size_t count);
ParseRes        pr_fail();
// allocate nodes
Node*           alloc_node(ParserCtx* pctx);
Node*           new_node(ParserCtx* pctx, NodeType type, Token token);
int                       ast_add_node(AST* ast, Node* n);
Node*           arena_add_node(Arena* a, Node n);


// parser tokens
Token           current(ParserCtx* pctx);
Token           peek(ParserCtx* pctx);
Token           consume(ParserCtx* pctx);

// pctx fucntions
// returns 1 on success
ParserCtx*      pctx_new(Token* tokens, size_t tokens_count);
int             pctx_destry(ParserCtx* pctx);

// node stuff
int             is_cmpt_constant(Node* expr);


// symbolstore
SymbolStore*    ss_new(SymbolStore* parent);
Function*       ss_get_fn(SymbolStore* ss, Name name);
Variable*       ss_get_variable(SymbolStore* ss, Name name);
Type*           ss_get_type(SymbolStore* ss, Name name);
int             ss_new_var(SymbolStore* ss, Variable var);
int             ss_new_type(SymbolStore* ss, Type t);
int             ss_new_fn(SymbolStore* ss, Function fn);

SymbolType      ss_sym_exists(SymbolStore* ss, Name name);
// type helper
int             determinate_type(SymbolStore* ss, Type* _type);
Type            get_lowest_type(Type _t);

// print function/helpter
void            print_ast(AST* ast);
void            print_indent(size_t k);

const char*     optype_to_string(OpType op);
const char*     unary_type_to_string(UnaryType type);


void            print_node(Node* node, int indent);
const char*     node_type_to_string(NodeType type);
const char*     get_node_data(Node* node);
char* print_type_to_buffer(char* buf, const Type* type);


const char*     get_sym_type(SymbolType st);

void            print_type(const Type* type, int indent);
Type*           get_type_from_name_(Name name);
Name*           get_name_from_type_(Type t);

void            print_symbol(const Symbol* sym, int indent);
void            print_symbol_store(const SymbolStore* store, int indent);
void _cmptime_log_caller(const char *fmt, ...);

// error functions?
void err_sym_exists(Name name);
// get sym type
// returns 1 on true

#endif // PARSER_H
