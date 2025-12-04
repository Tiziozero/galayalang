#ifndef NODE_H
#define  NODE_H
#include "token.h"
typedef struct Node Node;
typedef enum {
    NODE_NONE,
    NODE_VAR_DECLRETATION,
} NodeKind;
typedef struct NodeList NodeList;
struct Node{
    NodeKind kind;
    const Token* token;
    union {
        struct {
            usize size;
            Name name;
            usize type;
        } var_recleration;
    } ;
};
#endif // NODE_H
