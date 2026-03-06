#include "parser.h"

typedef struct TypeChecker TypeChecker;
struct TypeChecker {
};
int type_check_node(Parser* p, TypeChecker* tc, Node* n);
int type_check(Parser* p) {
    if (!p) {
        return 0;
    }

    size_t errs= 0;
    TypeChecker tc;
    for (size_t i = 0; i < p->nodes_count; i++) {
        if (!type_check_node(p, &tc, p->nodes[i])) {
            err("Failed to type check node %i.", i);
        }
    }
    return errs == 0;
}
#define CASE(NodeKind, scope) case Node##NodeKind: scope; break;
int type_check_node(Parser* p, TypeChecker* tc, Node* n) {
    int errs = 0;
    switch (n->kind) {
CASE(VarDec,
{
    return 0;
})
        default:
            panic("Unhandled node %d.", n->kind);
            return 0;
    }
    return errs == 0;
}
