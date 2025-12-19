#ifndef PARSER_H
#define PARSER_H
#include "lexer.h"
#include "utils.h"
typedef enum {
    NodeNone,
    NodeNumLit,
    NodeAssignment,
    NodeAddition,
    NodeSubtraction,
    NodeMultiplication,
    NodeDividion,
    NodeFnDec,
    NodeFnCall,

    NodeVarDec,
} NodeType;

typedef struct Node Node;
struct Node {
    NodeType type;
    union {
        Name var_dec; // change once type are implemented?
        double number;
        struct {
            Name target;
            Node* value;
        } assignment;
        struct {
            Node* left;
            Node* right;
        } addition;
        struct {
            Node* left;
            Node* right;
        } sub;
        struct {
            Node* left;
            Node* right;
        } mlt;
        struct {
            Node* left;
            Node* right;
        } dvd;
        struct {
            Name name;
            Node* body;
            // add type and args
        } fn_dec;
        struct {
            Node* nodes[100]; // limit to 100 for now cus icba
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
