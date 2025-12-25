#ifndef PARSER_H
#define PARSER_H
#include <stdlib.h>
#include <unistd.h>
#include "lexer.h"
#include "utils.h"
typedef enum {
    NodeNone,

    NodeCast,
    NodeVarDec,
    NodeVar,
    NodeField,
    NodeIndex,
    NodeUnary,
    NodeNumLit,
    // NodeAssignment,
    NodeBinOp,
    NodeFnDec,
    NodeIfElse,
    NodeFnCall,
    NodeBlock,
    NodeRet,
} NodeType;
static inline const char* node_type_to_string(NodeType type) {
    switch (type) {
        case NodeNone:        return "None";
        case NodeVarDec:      return "VarDec";
        case NodeNumLit:      return "NumLit";
        case NodeUnary:      return "NodeUnary";
        case NodeBinOp:       return "BinOp";
        case NodeFnDec:       return "FnDec";
        case NodeFnCall:      return "FnCall";
        case NodeBlock:       return "Block";
        default:              return "Unknown";
    }
}
typedef enum {
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
} OpType;




static const size_t ptr_size = sizeof(void*);

typedef enum {
    struct_t,
    enum_t,
    union_t,
} aggregate_types;

typedef enum {
    signed_t,
    unsigned_t,
    _float_t,
    ptr_t,
    aggregate_t,
    void_t,
    none_t,
} Type_type;


typedef struct type_t type_t;
struct type_t {
    Type_type t;
    size_t size; // for number (signed_t, unsigned_t, float_t)
    union {
        void* data; // todo. for aggregate types
        type_t* ptr;
    };
};
static struct{type_t type; Name n;} known_types[] = {
    {(type_t){.t=signed_t, .size=1}, (Name){"char", 4}},

    {(type_t){.t=signed_t, .size=1}, (Name){"i8", 2}},
    {(type_t){.t=signed_t, .size=2}, (Name){"i16", 3}},
    {(type_t){.t=signed_t, .size=4}, (Name){"i32", 3}},
    {(type_t){.t=signed_t, .size=8}, (Name){"i64", 3}},
    {(type_t){.t=signed_t, .size=16}, (Name){"i128", 4}},

    {(type_t){.t=unsigned_t, .size=1}, (Name){"u8", 2}},
    {(type_t){.t=unsigned_t, .size=2}, (Name){"u16", 3}},
    {(type_t){.t=unsigned_t, .size=4}, (Name){"u32", 3}},
    {(type_t){.t=unsigned_t, .size=8}, (Name){"u64", 3}},
    {(type_t){.t=unsigned_t, .size=16}, (Name){"u128", 4}},

    {(type_t){.t=unsigned_t, .size=ptr_size}, (Name){"usize", 5}},
};

static inline type_t* get_type_from_name(Name name) {
    for (int i = 0; i < sizeof(known_types)/sizeof(known_types[0]); i++) {
        if (name_cmp(name, known_types[i].n)) {
            return &known_types[i].type;
        }
    }
    return NULL;
}
static inline Name* get_name_from_type(type_t t) {
    for (int i = 0; i < sizeof(known_types)/sizeof(known_types[0]); i++) {
        if (known_types[i].type.t == t.t
            && known_types[i].type.size == t.size) {
            return &known_types[i].n;
        }
    }
    return NULL;
}


typedef struct Node Node;
struct Node {
    NodeType type;
    type_t expr_type;
    union {
        struct {
            type_t to;
            Node* expr;
        } cast; // change once type are implemented?
        struct {
            Name name;
            Node* value;
        }var_dec; // change once type are implemented?
        struct {
            Name name;
        }var; // change once type are implemented?
        struct {
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
            // add type and args
        } fn_dec;
        struct {
            Node** conditions; // more if blocks
            Node** blocks;
            size_t count;
            Node* else_block;
        } if_else_con;
        struct {
            Node* fn; // name will be a node/expression
            Node** args;
            size_t args_count;
            // add type and args
        } fn_call;
        struct {
            Node** nodes;
            size_t nodes_count;
        } block;
        Node* ret; // expression
    };
} ;

typedef struct {
    Node** nodes; // array of nodes
    size_t nodes_count;
    size_t max_nodes;
    Arena* arena;
} AST;


static inline const char* optype_to_string(OpType op) {
    switch (op) {
        case OpAdd:     return "Add";
        case OpSub:     return "Sub";
        case OpMlt:     return "Mul";
        case OpDiv:     return "Div";
        case OpMod:     return "Mod";

        case OpAnd:     return "BitAnd";
        case OpOr:      return "BitOr";
        case OpXor:     return "BitXor";

        case OpLSh:     return "LShift";
        case OpRSh:     return "RShift";

        case OpOrOr:    return "LogicalOr";
        case OpAndAnd:  return "LogicalAnd";

        case OpEq:      return "Equal";
        case OpNeq:     return "NotEqual";

        case OpLt:      return "Less";
        case OpGt:      return "Greater";
        case OpLe:      return "LessEqual";
        case OpGe:      return "GreaterEqual";

        case OpAssign:  return "Assign";

        case OpNone:    return "None";

        default:
            err("unknown op type: %d", op);
            assert(0);
            return "Unknown";
    }
}

static inline void print_indent(size_t k) {
    for (int i = 0; i < k; i++) printf(" ");
}
static inline void print_node(Node* node, int indent) {
    if (!node) {
        printf("%*sNULL\n", indent, "");
        return;
    }
    print_indent(indent);
    
    
    switch (node->type) {
        case NodeRet:
            printf("return:\n");
            print_node(node->ret, indent+2);
            break;
        case NodeNone:
            printf("None\n");
            break;
            
        case NodeVarDec:
            printf("VarDec: ");
            print_name(node->var_dec.name);
            if (node->var_dec.value != NULL) {
                print_node(node->var_dec.value,indent+2);
            }
            break;
            
        case NodeVar:
            printf("Var: ");
            print_name(node->var.name);
            break;
            
        case NodeNumLit:
            printf("NumLit: %g\n", node->number.number);
            break;
            
        case NodeUnary:
            printf("Unary: %d\n", node->unary.type);
            print_node(node->unary.target, indent+2);
            break;
        /*case NodeAssignment:
            printf("Assignment: \n");
            print_node(node->assignment.target, indent+2);
            print_node(node->assignment.value, indent + 2);
            break;*/
            
        case NodeBinOp:
            printf("BinOp: %s\n", optype_to_string(node->binop.type));
            print_node(node->binop.left, indent + 2);
            print_node(node->binop.right, indent + 2);
            break;
            
        case NodeFnDec:
            printf("FnDec: ");
            print_name(node->fn_dec.name);
            print_node(node->fn_dec.body, indent + 2);

            break;
            
        case NodeFnCall:
            printf("FnCall: ");
            printf(" %zu args\n", node->fn_call.args_count);
            print_node(node->fn_call.fn, indent+2);
            if (node->fn_call.args_count > 0) {
                print_indent(indent+2); printf("args:\n");
            }
            for (int i = 0; i < node->fn_call.args_count; i++) {
                print_node(node->fn_call.args[i], indent+4);
                // printf("\n");
            }
            break;
            
        case NodeBlock:
            printf("Block (%zu nodes)\n", node->block.nodes_count);
            for (size_t i = 0; i < node->block.nodes_count; i++) {
                print_node(node->block.nodes[i], indent + 2);
            }
            break;
            
        default:
            err("\nUnknown node type: %d.", node->type);
            break;
    }
}


static inline void print_ast(AST* ast) {
    if (!ast) {
        printf("NULL AST\n");
        return;
    }
    
    printf("AST (%zu/%zu nodes)\n", ast->nodes_count, ast->max_nodes);
    for (size_t i = 0; i < ast->nodes_count; i++) {
        printf("[%zu] ", i);
        print_node(ast->nodes[i], 0);
    }
    info("For a total of %zu node allocations (?).",
         ast->arena->node_allocations);
}

static inline int ast_add_node(AST* ast, Node* n) {
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

static inline Node* arena_add_node(Arena* a, Node n) {
    a->node_allocations++;
    return (Node*)arena_add(a, sizeof(Node), &n);
}

typedef enum {
    PrOk, // one node
    PrFail,
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


static inline ParseRes pr_ok(Node* n) {
    return (ParseRes){.ok=PrOk,.node=n};
}
static inline ParseRes pr_ok_many(Node* nodes[10], size_t count) {
    ParseRes pr;
    pr.ok = PrOk;
    pr.many.count = count;
    memcpy(pr.many.nodes, nodes, count*sizeof(Node*));
    return pr;
}
static inline ParseRes pr_fail() {
    return (ParseRes){PrFail,NULL};
}

// returns 1 on true
static inline int is_cmpt_constant(Node* expr) {
    switch (expr->type) {
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
            err("can not determinale compile time value of expression: %d.",
                expr->type); return 0;
    }
    return 1;
}

static inline const char* get_node_data(Node* node) {
    static char buf[128];

    if (!node) {
        snprintf(buf, sizeof(buf), "NULL");
        return buf;
    }

    switch (node->type) {
        case NodeNumLit:
            snprintf(buf, sizeof(buf),
                     "%s(%g)",
                     node_type_to_string(node->type),
                     node->number.number);
            break;

        case NodeBinOp:
            snprintf(buf, sizeof(buf),
                     "%s(%s)",
                     node_type_to_string(node->type),
                     optype_to_string(node->binop.type));
            break;

        case NodeVar:
            snprintf(buf, sizeof(buf),
                     "%s(%.*s)",
                     node_type_to_string(node->type),
                     (int)node->var.name.length,
                     node->var.name.name);
            break;

        case NodeVarDec:
            snprintf(buf, sizeof(buf),
                     "%s(%.*s)",
                     node_type_to_string(node->type),
                     (int)node->var_dec.name.length,
                     node->var_dec.name.name);
            break;

        default:
            snprintf(buf, sizeof(buf),
                     "%s",
                     node_type_to_string(node->type));
            break;
    }

    return buf;
}

typedef struct {
    Name name;
    type_t type;
} Variable;
typedef struct {
    Name name;
    type_t type;
} Argument;

typedef struct {
    Name name;
    Argument* args;
    size_t args_count;
    size_t args_capacity;
    type_t return_type;
} Function;
typedef struct {
    Variable* vars;
    size_t vars_count;
    size_t vars_capacity;
    Function* fns;
    size_t fns_count;
    size_t fns_capacity;
} Name_Store;

typedef struct {
    AST* ast;
    Name_Store names;
    Arena gpa; // general purpose arena
    Token* tokens;
    size_t tokens_count;
    size_t tokens_index;
} ParserCtx;

static inline Token current(ParserCtx* pctx) {
    Token t;
    if (pctx->tokens_index >= pctx->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = pctx->tokens[pctx->tokens_index];
        // pctx->tokens_index += 1;
    }
    return t;
}
static inline Token peek(ParserCtx* pctx) {
    Token t;
    if (pctx->tokens_index + 1 >= pctx->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = pctx->tokens[pctx->tokens_index + 1];
    }
    return t;
}
static inline Token consume(ParserCtx* pctx) {
    Token t;
    if (pctx->tokens_index >= pctx->tokens_count) {
        t = (Token){ .type=TokenEOF };
    } else {
        t = pctx->tokens[pctx->tokens_index];
        info("\t\t%s", get_token_data(t));
        pctx->tokens_index += 1;
    }
    return t;
}


static inline ParserCtx* pctx_new(Token* tokens, size_t tokens_count) {
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

    Name_Store ns;
    ns.fns_capacity = 256;
    ns.fns = (Function*)malloc(ns.fns_capacity*sizeof(Function));
    if (!ns.fns) {
        free(ast);
        free(arena);
        free(pctx);
        return NULL;
    }
    ns.fns_count = 0;
    ns.vars_capacity = 256;
    ns.vars = (Variable*)malloc(ns.fns_capacity*sizeof(Variable));
    if (!ns.vars) {
        free(ns.fns);
        free(ast);
        free(arena);
        free(pctx);
        return NULL;
    }
    ns.vars_count = 0;

    pctx->names = ns;
    pctx->gpa = arena_new(1024, sizeof(void*));

    pctx->tokens = tokens;
    pctx->tokens_count = tokens_count;
    pctx->tokens_index = 0;
    return pctx;
}
// returns 1 on success
static inline int pctx_destry(ParserCtx* pctx) {
    if (!pctx) return 0;
    for (size_t i = 0; i < pctx->ast->arena->pages_count; i++) {
        free(pctx->ast->arena->pages[i]);
    }

    free(pctx->ast->arena->pages);
    free(pctx->ast->arena);
    free(pctx->ast->nodes);
    free(pctx->ast);

    for (size_t i = 0; i < pctx->names.fns_count; i++) {
        Function f = pctx->names.fns[i];
        if (f.args) {
            free(f.args);
        }
    }
    free(pctx->names.fns);

    for (size_t i = 0; i < pctx->names.vars_count; i++) {
    }
    free(pctx->names.vars);
    for (size_t i = 0; i < pctx->gpa.pages_count; i++) {
        free(pctx->gpa.pages[i]);
    }
    free(pctx);
    return 1;
}


static inline Variable* pctx_get_variable(ParserCtx* pctx, Name name) {
    Variable* vars =  pctx->names.vars;
    for (size_t i = 0; i < pctx->names.vars_count; i++) {
        if (name_cmp(name, vars[i].name)) {
            return &vars[i];
        }
    }
    return NULL;
}

ParserCtx* parse(Lexer* l);
#endif // PARSER_H
