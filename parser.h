#ifndef PARSER_H
#define PARSER_H
#include <stdlib.h>
#include "lexer.h"
#include "utils.h"
typedef enum {
    NodeNone,

    NodeVarDec,
    NodeVar,
    NodeField,
    NodeIndex,
    NodeReference,
    NodeDereference,
    NodeNumLit,
    NodeAssignment,
    NodeBinOp,
    NodeFnDec,
    NodeFnCall,
    NodeBlock,
    NodeAndAnd,
    NodeOrOr,
} NodeType;
static inline const char* node_type_to_string(NodeType type) {
    switch (type) {
        case NodeNone:        return "None";
        case NodeVarDec:      return "VarDec";
        case NodeReference:   return "Reference";
        case NodeDereference: return "Dereference";
        case NodeNumLit:      return "NumLit";
        case NodeAssignment:  return "Assignment";
        case NodeBinOp:       return "BinOp";
        case NodeFnDec:       return "FnDec";
        case NodeFnCall:      return "FnCall";
        case NodeBlock:       return "Block";
        case NodeAndAnd:      return "AndAnd";
        case NodeOrOr:        return "OrOr";
        default:              return "Unknown";
    }
}
typedef enum {
    OpAdd,
    OpSub,
    OpMlt,
    OpDiv,
    OpMod,
    OpAnd,
    OpOr,
    OpXor,
    OpLSh,
    OpRSh,
}OpType;

typedef struct Node Node;
struct Node {
    NodeType type;
    union {
        struct {
            Name name;
        }var_dec; // change once type are implemented?
        struct {
            Name name;
        }var; // change once type are implemented?
        struct {
            Node* target;
        } ref;
        struct {
            Node* target;
        } deref;
        struct {
            double number;
        } number;
        struct {
            Node* target;
            Node* value;
        } assignment;
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
            Name name;
            // add type and args
        } fn_call;
        struct {
            Node** nodes; // limit to 100 for now cus icba
            size_t nodes_count;
        } block;
    };
} ;

typedef struct {
    Node** nodes; // array of nodes
    size_t nodes_count;
    size_t max_nodes;
    Arena* arena;
} AST;

AST* parse(Lexer* l, Arena* a);

static inline const char* optype_to_string(OpType op) {
    switch (op) {
        case OpAdd: return "Add";
        case OpSub: return "Sub";
        case OpMlt: return "Mlt";
        case OpDiv: return "Div";
        case OpMod: return "Mod";
        case OpAnd: return "And";
        case OpOr:  return "Or";
        case OpXor: return "Xor";
        case OpLSh: return "LShift";
        case OpRSh: return "RShift";
        default:    err("unknown op type: %zu",op); assert(0); return "Unknown";
    }
}

static inline void print_node(Node* node, int indent) {
    if (!node) {
        printf("%*sNULL\n", indent, "");
        return;
    }
    
    for (int i = 0; i < indent; i++) printf(" ");
    
    switch (node->type) {
        case NodeNone:
            printf("None\n");
            break;
            
        case NodeVarDec:
            printf("VarDec: ");
            print_name(node->var_dec.name);
            break;
            
        case NodeVar:
            printf("Var: ");
            print_name(node->var.name);
            break;
            
        case NodeReference:
            printf("Reference to \n");
            print_node(node->ref.target, indent+2);
            break;
            
        case NodeDereference:
            printf("Dereference location: \n");
            print_node(node->deref.target, indent+2);
            break;
            
        case NodeNumLit:
            printf("NumLit: %g\n", node->number.number);
            break;
            
        case NodeAssignment:
            printf("Assignment: \n");
            print_node(node->assignment.target, indent+2);
            print_node(node->assignment.value, indent + 2);
            break;
            
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
            print_name(node->fn_call.name);
            break;
            
        case NodeBlock:
            printf("Block (%zu nodes)\n", node->block.nodes_count);
            for (size_t i = 0; i < node->block.nodes_count; i++) {
                print_node(node->block.nodes[i], indent + 2);
            }
            break;
            
        case NodeAndAnd:
            printf("LogicalAnd\n");
            // Add left/right fields if needed
            break;
            
        case NodeOrOr:
            printf("LogicalOr\n");
            // Add left/right fields if needed
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
    info("For a total of %zu node allocations (?).", ast->arena->node_allocations);
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
    ParseRes pr = (ParseRes){.ok=PrOk,.many={.count=count}};
    memcpy(pr.many.nodes, nodes, 10*sizeof(Node*));
    return pr;
}
static inline ParseRes pr_fail() {
    return (ParseRes){PrFail,NULL};
}



#endif // PARSER_H
