#ifndef PARSER_H
#define PARSER_H
#include "lexer.h"
#include "utils.h"
typedef enum {
    NodeNone,

    NodeVarDec,
    NodeVar,
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

#endif // PARSER_H
