#ifndef PARSER_H
#define PARSER_H
#include <stdlib.h>
#include <unistd.h>
#include "lexer.h"
#include "utils.h"
typedef struct SymbolStore SymbolStore;
typedef enum {
    NodeNone,

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
    NodeBlock,
    NodeRet,
    NodeTypeData, // type node
} NodeType;

static inline const char* node_type_to_string(NodeType type) {
    switch (type) {
    case NodeNone:      return "NodeNone";
    case NodeCast:      return "NodeCast";
    case NodeVarDec:    return "NodeVarDec";
    case NodeVar:       return "NodeVar";
    case NodeField:     return "NodeField";
    case NodeIndex:     return "NodeIndex";
    case NodeUnary:     return "NodeUnary";
    case NodeNumLit:    return "NodeNumLit";
    case NodeArg:       return "NodeArg";
    case NodeBinOp:     return "NodeBinOp";
    case NodeFnDec:     return "NodeFnDec";
    case NodeIfElse:    return "NodeIfElse";
    case NodeFnCall:    return "NodeFnCall";
    case NodeBlock:     return "NodeBlock";
    case NodeRet:       return "NodeRet";
    case NodeTypeData:  return "NodeType";
    default:            return "<unknown NodeType>";
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

// types
typedef enum {
    tt_to_determinate,
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
    tt_char,
    tt_ptr,
    tt_array,
    tt_aggregate,
    tt_void,
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
        } array;
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
            Type type;
        } arg;
        struct {
            Type to;
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
            Type return_type;
            // type
            Node** args;
            size_t arg_count;
            // add type and args
        } fn_dec;
        struct {
            Node** conditions; // more if blocks
            Node** blocks;
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
        Type type_data;
        Node* ret; // expression
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
    (Type){.type=tt_ptr, .size=ptr_size, .name=(Name){"ptr", 3}},
};

static inline Type* get_type_from_name(Name name) {
    for (int i = 0; i < sizeof(known_types)/sizeof(known_types[0]); i++) {
        if (name_cmp(name, known_types[i].name)) {
            return &known_types[i];
        }
    }
    return NULL;
}
static inline Name* get_name_from_type(Type t) {
    for (int i = 0; i < sizeof(known_types)/sizeof(known_types[0]); i++) {
        if (known_types[i].type == t.type
            && known_types[i].size == t.size) {
            return &known_types[i].name;
        }
    }
    err("type not known");
    return NULL;
}

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
            
        case NodeFnCall: {
            Name name = node->fn_call.fn_name;
            printf("FnCall (%.*s):", (int)name.length, name.name);
            printf(" %zu args\n", node->fn_call.args_count);
            // print_node(node->fn_call.fn, indent+2);
            if (node->fn_call.args_count > 0) {
                print_indent(indent+2); printf("args:\n");
            }
            for (int i = 0; i < node->fn_call.args_count; i++) {
                print_node(node->fn_call.args[i], indent+4);
                // printf("\n");
            }
            break;
        }
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
    pr.ok = PrMany;
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
} Function;

typedef enum {
    SymNone = 0, // fail
    SymVar,
    SymFn,
    SymArg,
    SymType,
    SymField,
} SymbolType;
static inline const char* get_sym_type(SymbolType st) {
    switch (st) {
        case SymVar: return "Variable (SymVar0";
        case SymFn: return "Function (SymFn)";
        case SymArg: return "Argument (SymArg)";
        case SymType: return "Type 9SymType)";
        case SymField: return "Field (SymField)";
        default:err("invalid symbol type %d.", st);
    }
    return NULL;
}

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
        // info("\t\tconsume %s", get_token_data(t));
        pctx->tokens_index += 1;
    }
    return t;
}

static int err_type_already_exists   = 1;
static int err_failed_realloc        = 2;
// returns 0 on success cus errors
static inline SymbolType ss_sym_exists(SymbolStore* ss, Name name) {
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
// returns 1 on success
static inline int ss_new_var(SymbolStore* ss, Variable var) {
    // pre-requirements
    if (var.type.type == tt_to_determinate) return 0;
    if (var.name.name == 0 || var.name.length == 0) return 0;
    char name_buf[100];
    print_name_to_buf(name_buf, 100, var.name);
    char type_buf[100];
    print_name_to_buf(type_buf, 100, var.type.name);
    info("type going in: %s", type_buf);
    // dbg("type \"%s\".", buf);
    // check if it exists
    if (ss_sym_exists(ss, var.name)) {
        err("var exists");
        return 0;
    }
    Type check_type = var.type;
    while (check_type.type == tt_ptr || check_type.type == tt_array) {
        if (check_type.type == tt_ptr) {
        info("%s is ptr", name_buf);
            check_type = *check_type.ptr;
        } else if (check_type.type == tt_array) {
            info("%s is arr", name_buf);
            check_type = *check_type.array.type;
        }
        if (!(check_type.type == tt_ptr || check_type.type == tt_array)) {
            char name_buf[100];
            print_name_to_buf(name_buf, 100, check_type.name);
            info("\tto %s.", name_buf);
        } 
    }
    SymbolType res = ss_sym_exists(ss, check_type.name);
    if (res != SymType) {
        char buf[100];
        print_name_to_buf(buf, 100, var.type.name);
        err(" === Undefined type \"%s\". === ", buf);
        if (res != SymNone) {
            info("\t got %s instead.", res);
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
static inline int ss_new_type(SymbolStore* ss, Type t) {
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
static inline int ss_new_fn(SymbolStore* ss, Function fn) {
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
    return 1;
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
    for (size_t i = 0; i < sizeof(known_types)/sizeof(known_types[0]); i++) {
        char buf[100];
        print_name_to_buf(buf, 100, known_types[i].name);
        dbg("Adding type \"%s\".", buf);
        if (ss_new_type(&pctx->symbols, known_types[i])) {
            err("Failed to add type: %s", buf);
            assert(0);
        }
    }
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

    for (size_t i = 0; i < pctx->symbols.syms_count; i++) {
        Symbol s = pctx->symbols.syms[i];
        if (s.sym_type == SymFn) {
            Function f = s.fn;
            if (f.args) {
                free(f.args);
            }
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


static inline Variable* ss_get_variable(SymbolStore* ss, Name name) {
    for (size_t i = 0; i < ss->syms_count; i++) {
        if (ss->syms[i].sym_type == SymVar) {
            Variable* var = &ss->syms[i].var;
            if (name_cmp(name, var->name)) {
                return var;
            }
        }
    }
    return NULL;
}
static inline Function* ss_get_fn(SymbolStore* ss, Name name) {
    for (size_t i = 0; i < ss->syms_count; i++) {
        if (ss->syms[i].sym_type == SymFn) {
            Function* f = &ss->syms[i].fn;
            if (name_cmp(name, f->name)) {
                return f;
            }
        }
    }
    return NULL;
}
static inline Type* ss_get_type(SymbolStore* ss, Name name) {
    for (size_t i = 0; i < ss->syms_count; i++) {
        if (ss->syms[i].sym_type == SymType) {
            Type* t = &ss->syms[i].type;
            if (name_cmp(name, t->name)) {
                return t;
            }
        }
    }
    return NULL;
}


static inline Node* alloc_node(ParserCtx* pctx) {
    Node n;
    n.token = (Token){TokenEOF, 0};
    n.type = NodeNone;
    Node* ret_n = arena_add_node(pctx->ast->arena, n);
    return ret_n;
}
static inline Node* new_node(ParserCtx* pctx, NodeType type, Token token) {
    Node n;
    n.token = token;
    n.type = type;
    Node* ret_n = arena_add_node(pctx->ast->arena, n);
    return ret_n;
}


ParserCtx* parse(Lexer* l);
#endif // PARSER_H
