#ifndef NODE_H
#define  NODE_H
#include "token.h"
#include "map.c"
#include <gala.h>
typedef struct Node Node;
typedef enum {
    NODE_NONE,
    NODE_VAR_DECLRETATION,
    NODE_VAR_ASSIGNMENT,
    NODE_VAR,
    NODE_OP,
    NODE_NUM_LITERAL,
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
            Token op; 
            Node* left;
            Node* right;
        } operation;
        Name var;
        struct {
            double n;
        } num_literal;
    } ;
};
static inline Node* nodea(Arena* a, Node node) {
    Node* p = (Node*)aalloc(a,sizeof(node));
    *p = node;
    return p;
}
#include <stdio.h>

static void print_indent(int depth) {
    for (int i = 0; i < depth; i++) printf("  ");
}

inline static const char* op_to_string(TokenType t) {
    switch (t) {
        case TOKEN_OP_PLUS:  return "+";
        case TOKEN_OP_MINUS: return "-";
        case TOKEN_STAR:     return "*";
        case TOKEN_F_SLASH:  return "/";
        default:             return "?";
    }
}

inline static void print_node(const Node* node, int depth) {
    if (!node) {
        print_indent(depth);
        printf("(null)\n");
        return;
    }

    print_indent(depth);

    switch (node->kind) {

    case NODE_VAR_DECLRETATION:
        printf("VarDecl name='%.*s' type='%.*s' size=%zu\n",
            (int)node->var_recleration.name.length,
            node->var_recleration.name.name,
            (int)node->var_recleration.type.length,
            node->var_recleration.type.name,
            node->var_recleration.size
        );
        break;

    case NODE_VAR: {
        print_indent(depth);
            write_buffer_of_len(node->var.name,
                                node->var.length);
            println("");
            break;
        }
    case NODE_VAR_ASSIGNMENT: {
        print_indent(depth);
            printf("Assignment: ");
            write_buffer_of_len(node->assignment.target.name,
                                node->assignment.target.length);
            printf(" =\n");
            print_node(node->assignment.value,depth+1);
            break;
    }
    case NODE_OP: {
        TokenType top = node->operation.op.type;
        const char* op = op_to_string(top);
        printf("Op (%s)\n", op);

        print_indent(depth);
        printf("Left:\n");
        print_node(node->operation.left, depth + 1);

        print_indent(depth);
        printf("Right:\n");
        print_node(node->operation.right, depth + 1);

        break;
    }
    case NODE_NUM_LITERAL: {
        print_indent(depth);
            printf("%lf\n", node->num_literal.n);

        break;
    }

    case NODE_NONE:
        printf("NodeNone\n");
        break;

    default:
        printf("UnknownNodeKind(%d)\n", node->kind);
        break;
    }
}

#endif // NODE_H
