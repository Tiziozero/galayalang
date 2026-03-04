#include "logger.h"
#include "parser.h"
#include "utils.h"
int symbols(Parser* p, Node* n);
int resolve_symbols(Parser* p) {
    if (!p) {
        return 0;
    }

    size_t errs= 0;
    for (size_t i = 0; i < p->nodes_count; i++) {
        if (!symbols(p, p->nodes[i])) {
            err("Failed to resolve symbols for node %i.", i);
        }
    }
    return errs == 0;
}
int symbols(Parser* p, Node* n) {
    if (!n) return 0;
    switch (n->kind) {
        case NodeVarDec:
            {
                if (!is_valid_name(n->var_dec.symbol->symbol)) {
                    panic("invalid name in vardec. shouldn't happen.");
                    return 0;
                }
                if (!n->var_dec.type) {
                    panic("No type. inference not implemented.");
                    return 0;
                }
                if (n->var_dec.value) {
                    if (symbols(p, n->var_dec.value)) {
                        err("Failed to resolve symbols for vardec  value.");
                        return 0;
                    }
                }
                Variable v;
                v.name = n->var_dec.symbol->symbol;
                v.type = n->var_dec.type->type_data;
                Symbol* var_sym = st_add_var(p->syms, v);
                if (!var_sym) {
                    err("failed to create variable symbol.");
                    return 0;
                }
                dbg("Vardec ok.");
            } break;
        default: TODO("resolve symbol.");
    }
    return 1;
}
