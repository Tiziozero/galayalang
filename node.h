#ifndef NODE_H
#define  NODE_H
#include "token.h"
#include "map.c"
typedef struct Node Node;
typedef enum {
    NODE_NONE,
    NODE_VAR_DECLRETATION,
} NodeKind;


typedef enum {
    OP_ADD,
    OP_SUB,
    OP_MULT,
    OP_DIV,
} Operation;
typedef struct NodeList NodeList;
struct Node{
    NodeKind kind;
    const Token* token;
    union {
        struct {
            usize size;
            Name name;
            Name type;
        } var_recleration;
        struct {
            Name target;
            Node* value;
        } assignment;
        struct {
            Operation op; 
            Node* left;
            Node* right;
        } operation;
    } ;
};
static inline Node* nodea(Arena* a, Node node) {
    Node* p = (Node*)arena_alloc(a,sizeof(node));
    *p = node;
    return p;
}
#endif // NODE_H
