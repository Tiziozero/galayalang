#include "parser.h"
#include "utils.h"

/*

int is_int_type(Type t) {
    return t.type == tt_unsigned
        || t.type == tt_signed;
}
int is_float_type(Type t) {
    return t.type == tt_float;
}
int is_ptr_type(Type t) {
    return t.type == tt_ptr;
}

Type common_type(OpType op, Type left, Type right) {
    switch (op) {
        case OpAdd: case OpSub: {
            if (is_ptr_type(left) && is_ptr_type(right))
                return (Type){.type=tt_unsigned, .size=ptr_size};
            // pointers converge to pointers
            if (is_ptr_type(left) || is_ptr_type(right))
                return (Type){.type=tt_ptr};

            // both int or float -> return larger sized one
            else if ((is_int_type(left) && is_int_type(right))
            || (is_float_type(left) && is_float_type(right)))
                return left.size > right.size ? left : right ;

            // one is a float
            else if ((is_float_type(left) && is_int_type(right))
                || (is_int_type(left) && is_float_type(right))
                || (is_float_type(left) && is_float_type(right))) {
                size_t size = left.size > right.size ? left.size : right.size;
                return (Type){.type = tt_float, .size = size };
            }
        } break;
        default: err("unknown op");
    }
    return (Type){tt_none};
}

const int err_can_not_determinate   = 1;
const int err_invalid_cast          = 2;
int get_expression_type(ParserCtx* pctx, Node* expr, Type* t) {
    Type ret_t;
    switch (expr->type) {
        case NodeBinOp: {
            Type left_t, right_t;
            int l = get_expression_type(pctx, expr->binop.left, &left_t);
            if (l != 0) return l;
            int r = get_expression_type(pctx, expr->binop.right, &right_t);
            if (r != 0) return r;

            if (r == l) { *t = left_t; return 0; }
            *t = common_type(expr->binop.type, left_t,  right_t);
            return 0;
        } break;
        case NodeNumLit: {
            t->type = tt_signed;
            t->size = 0;
            return 0;
        } break;
        case NodeVar: {
            Variable* v = pctx_get_variable(pctx, expr->var.name);
            if (!v) {
                char buf[100];
                err("Undecclared/Unknown variable \"%s\".", print_name_to_buf(buf,100,expr->var.name));

            }
            return err_can_not_determinate;
        } break;
        default:
            err("Cannot determinate type of expresssion.");
            return err_can_not_determinate;
    }
}
*/
