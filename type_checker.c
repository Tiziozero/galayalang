#include "type_checker.h"
#include "parser.h"
#include "print.h"
#include "utils.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>

int can_reference(Node* n) {
	// if (n->kind == NodeNumLit) return 0;
	if (n->type.type->type == tt_ptr) return 1; // can only deref pointers
    return 0;
}
Type* to_signed(TypeChecker* tc, Type* t) {
    if (!is_numeric(t)) return NULL;
    switch(t->type) {
        case tt_u8:     return ss_get_type(tc->ss, cstr_to_name("i8"));
        case tt_u16:    return ss_get_type(tc->ss, cstr_to_name("i16"));
        case tt_u32:    return ss_get_type(tc->ss, cstr_to_name("i32"));
        case tt_u64:    return ss_get_type(tc->ss, cstr_to_name("i64"));
        case tt_u128:   return ss_get_type(tc->ss, cstr_to_name("i128"));
        case tt_i8:     return ss_get_type(tc->ss, cstr_to_name("i8"));
        case tt_i16:    return ss_get_type(tc->ss, cstr_to_name("i16"));
        case tt_i32:    return ss_get_type(tc->ss, cstr_to_name("i32"));
        case tt_i64:    return ss_get_type(tc->ss, cstr_to_name("i64"));
        case tt_i128:   return ss_get_type(tc->ss, cstr_to_name("i128"));
        case tt_f32:    return ss_get_type(tc->ss, cstr_to_name("f32"));
        case tt_f64:    return ss_get_type(tc->ss, cstr_to_name("f64"));
        default: panic("Invalid type to convert to signed");
    }
    return NULL;
}

void print_two_types(Type* t1, Type* t2) {
    print_type(t1, 10);
    printf(" | ");
    print_type(t2, 10);
    printf("\n");
    fflush(stdout);
}

int cast(ParserCtx* pctx, Node** node, Type* type) {
    if (!pctx || !node || !type) return 0;
    Node* cast_node = new_node(pctx, NodeCast, (Token){0});
    if (!cast_node) return 0;
    cast_node->cast.to = type;
    cast_node->cast.expr = *node;
    *node = cast_node;
    return 1;
}

int type_cmp(Type* t1, Type* t2) {
    if (!t1) {
        err("type 1 is null");
        // assert(0&&"type 1 is null");
        return 0;
    }
    if (!t2) {
        err("type 2 is null");
        // assert(0&&"type 2 is null");
        return 0;
    }
    if (t1 == t2) return 1;
	if (!name_cmp(t1->name, t2->name)) return 0;
	return 1;
	// other stuff maybe for loose comparision
    if (t1->type != t2->type) return 0;
    // same type

    if (t1->type == tt_ptr) { // pointer types must be the same
        return type_cmp(t1->ptr, t1->ptr);
    }
    // after pointers/arrays cus they have no name and thus ts fails
    if (!name_cmp(t1->name, t2->name)) return 0; // same name

    return 1;
    TODO("implement rest");
}

// returns type to which to cast to (t1, t2 or null if none)
Type* can_implicit_cast(Type* t1, Type* t2) {
    if (type_cmp(t1, t2)) return t1;

    if (is_numeric(t1) && is_numeric(t2)) {
        if (is_float(t1) && is_float(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        if (is_float(t1) && !is_float(t2)) {
            return t1;
        }
        if (!is_float(t1) && is_float(t2)) {
            return t2;
        }
        if (is_signed(t1) && is_signed(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        if (is_unsigned(t1) && is_unsigned(t2)) {
            return t1->size > t2->size ? t1 : t2; // return larger one
        }
        TODO("handle all cases");
    }
    return NULL;
}

struct TypeChecker* new_tc(
        struct TypeChecker* tc, ParserCtx* pctx, SymbolStore* ss) {
    memset(tc, 0, sizeof(struct TypeChecker));
    if (!ss) {
        ss = ss_new(NULL);
    }
    tc->pctx = pctx;
    tc->ss = ss;
    tc->parent = tc ? tc : NULL;
    tc->ok = 1;
    return tc;
}

struct TypeChecker* new_scope(struct TypeChecker* parent, SymbolStore* ss) {
    struct TypeChecker* tc = malloc(sizeof(struct TypeChecker));
    memset(tc, 0, sizeof(struct TypeChecker));
    tc->parent = parent;
    tc->pctx = parent->pctx;
    tc->ss = ss;
    tc->ok = 1;
    return tc;
}

struct TypeChecker* tc_fn(struct TypeChecker* parent,
        SymbolStore* ss, Function* fn) {
    struct TypeChecker* tc = new_scope(parent, ss);
    tc->fn = fn;
    return tc;
}


int handle_binop_untyped(NodeTypeInfo* n1, NodeTypeInfo* n2) {
	if (n1->state == TsOk && n2->state == TsOk) return 1;
    if (state_is_untyped_number(n1->state)
			&& state_is_untyped_number(n2->state)) { // make sure both numbers
		if (n1->state == n2->state) return 1; // make sure they're the same
		if (n1->state == TsUntypedFloat) { // set to untyped float first
			n2->state = TsUntypedFloat;
		} else if (n2->state == TsUntypedFloat) {
			n1->state = TsUntypedFloat;
		} else TODO("Implement");

		return 0;
	}
    // if n1 is untyped and n2 is known
    if (state_is_untyped_number(n1->state) && n2->state == TsOk) {
        // if n2 is numeric
        if (is_numeric(n2->type)) {
            *n1 = *n2; // set n1 to n2 for now, later set to int/float
            return 1;
        } else {
            panic("can only compare untyped numbers for now.");
			return 0;
        }
    }
    if (state_is_untyped_number(n2->state) && n1->state == TsOk) {
        // if n1 is numeric
        if (is_numeric(n1->type)) {
            *n2 = *n1; // set n2 to n1 for now, later set to int/float
            return 1;
        } else {
            panic("can only compare untyped numbers for now.");
			return 0;
        }
    }
	err("Something else's wrong");
    return 0;
}
Type* arena_alloc_type(Arena* arena) {
	Type* t = arena_alloc(arena, sizeof(Type));
	if (!t) {
		err("Failed to allocate memory for type.");
		return NULL;
	}
	memset(t, 0, sizeof(Type));
	return t;
}
int type_check_expression(TypeChecker* tc, Node *node) {
    info("exprcheck %s", node_type_to_string(node->kind));
    switch (node->kind) {
        case NodeNumLit:
            {
				info("NumLit");
                int is_float = 0;
                node->type.type = NULL;
                node->type.state = TsOk;
                for (size_t i = 0; i < node->number.str_repr.length; i++) {   
                    // if it has a dot it's a float
                    // TODO: add a "get untyped type" fucntion or smth
                    if (node->number.str_repr.name[i] == '.')
                        is_float = 1;
                }
                if (is_float)
                    node->type.state = TsUntypedFloat;
                else
                    node->type.state = TsUntypedInt;
            } break;
        case NodeCast:
            {
                node->type.type = node->cast.to;
                // could be a struct for all I care
                if (!type_check_node(tc, node->cast.expr))
                    node->type.state = TsFailed;
                else
                    node->type.state = TsOk;
            } break;
        case NodeBinOp:
            {
                int errs = 0;
                // type check left/right branch
                if (!type_check_expression(tc, node->binop.left))
                    errs++;
                if (!type_check_expression(tc, node->binop.right))
                    errs++;
				if (errs > 0) {
					err("Failed typecheck in binop. %zu errors", errs);
					return 0;
				}
                // check if you can actually perform binop on types
                if (!can_binop(node->binop.left->type)
                        || !can_binop(node->binop.right->type)) {
                    print_two_types(node->binop.left->type.type,
                            node->binop.right->type.type);

					print_node(node->binop.right, 10);
					fflush(stdout);
					info("%s", node_type_to_string(node->binop.left->kind));
					print_node(node->binop.left, 10);
					fflush(stdout);
                    assert(0&&"Cannot binop types");
                }
                if (!handle_binop_untyped(&node->binop.left->type,
                        &node->binop.right->type)) {
                    err("Failed to handle untyped term in binop");
                    return 0;
                }
				// if both are untyped
				if (is_untyped(node->binop.left)
						&& is_untyped(node->binop.right)) {
					node->type = node->binop.left->type;
					break;
				}
                if (!type_cmp(node->binop.left->type.type,
                            node->binop.right->type.type)) {
                    err("fuckass buttfuck gay nigger binop "
                            "types don't maths.");
                    print_two_types(node->binop.left->type.type,
                            node->binop.right->type.type);
                    errs++;
                }
				if (errs == 0) {
					// set to left, should be same
					node->type = node->binop.left->type;
					break;
				} else {
					node->type.state = TsFailed;
					node->type.type = 0;
					return 0;
				}

            } break;
        case NodeUnary:
            {
                UnaryType type = node->unary.type;
                Node* target = node->unary.target;
                if (!type_check_expression(tc, target)) return 0;
                switch (type) {
                    case UnNegative:
                        if (is_untyped(target)) {
                            // untyped unsighed -> untuped int
                            if (target->type.state == TsUntypedUnsignedInt) {
                                node->type.state = TsUntypedInt;
                            }
                            // untyped int to untyped int
                            else if (target->type.state == TsUntypedInt) {
                                node->type.state = TsUntypedInt;
                            }
                            // untyped float to untyoed float
                            else if (target->type.state == TsUntypedFloat) {
                                node->type.state = TsUntypedFloat;
                            } else {
                                panic("Can't negate whatever ts this is.");
                            }
                        } else if (is_unsigned(target->type.type)) {
                            node->type.type = to_signed(tc,target->type.type);
                            node->type.state = TsOk;
                        } else if (is_float(target->type.type)
                                || is_signed(target->type.type)) {
                            node->type = target->type;
                        } else {
                            panic("Can't negate type.");
                        }
                        break;
                    case UnCompliment: // lazy approach ig.
                        if (!is_numeric(target->type.type)) {
                            err("Can only take the compiment "
                                    "of a numeric value."); return 0;
                        }
                        node->type = target->type;
                        break;
                    case UnNot:  // always untyped
                        if (!type_info_is_numeric(target->type)) {
                            err("Can only take the compiment "
                                    "of a numeric value."); return 0;
                        }
						node->type.type = NULL;
						node->type.state = TsUntypedInt;
						break;
                    case UnDeref:
                        {
                            if (!is_numeric(target->type.type)) {
                                err("Can only dereference numeric values.");
                            }
                            if (target->type.type->type == tt_ptr) {
                                node->type.type = target->type.type->ptr;
                                node->type.state = TsOk;
                            } else if (is_numeric(target->type.type)) {
                                node->type.type = NULL;
                                node->type.state = TsNeedsType;
                            } else {
                                err("Can only dereference numeric values.");
                            }
                        } break;
                    case UnRef:
                        {
                           if (!can_reference(target)) {
                               err("Can not reference %s",
                                       node_type_to_string(target->kind));
                               return 0;
                           }
						   Type* t = arena_alloc_type(&tc->pctx->gpa);
						   if (!t) {
							   panic("Cailed to allocate type in arena");
						   }
                           t->type = tt_ptr;
                           t->ptr = target->type.type;
                           node->type.type = t;
                           node->type.state = TsOk;
                        } break;
                    default: panic("unary not handled");
                }
            } break;
        case NodeVar:
        case NodeFnCall:
            return type_check_node(tc, node);
        default:panic("Gay expression/"
                        "node can not be part of an expression");
    }
    return 1;
}

int make_sure_everythings_ok_With_tc(TypeChecker* tc, Node *node) {
    return 0;
    size_t errs = 0;
    switch (node->kind) {
        case NodeVar:
            if (!node->var.type) {
                err("Variable is untyped."); errs++;
            } else {
                if (node->type.state != TsOk) {
                    err("Failed to typecheck variable."); errs++;
                } else if (node->type.type->type == tt_void) {
                    err("variables can't be void."); errs++;
                }
            }
            break;
        case NodeVarDec:
            if (node->type.state == TsFailed) {
                err("Failed to typecheck vardec."); errs++;}
            if (is_untyped(node)) {
                print_type(node->type.type, 10);
                err("vardec is untyped."); errs++;
            }
            if (node->var_dec.value) {
                errs += make_sure_everythings_ok_With_tc(
                        tc, node->var_dec.value); }
            break;
        case NodeFnDec:
            if (node->fn_dec.return_type == NULL)
                err("invalid fn return value."); errs++;
            for (size_t i = 0;
                    i < node->fn_dec.body->block.nodes_count; i++) {
                errs += make_sure_everythings_ok_With_tc(tc,
                        node->fn_dec.body->block.nodes[i]);
            }
            break;
        default:
            warn("Node %s %d", node_type_to_string(node->kind), node->kind);
            panic("Invalid node.");
    }
    return errs;
}
int type_check_node(TypeChecker* tc, Node *node) {
    dbg("Node check %s", node_type_to_string(node->kind));
    switch (node->kind) {
        case NodeVarDec:
            {
                int errs = 0;
                Type* var_type = node->var_dec.type;
                if (var_type->type == tt_void) { // type can not be void
                    err("Var type can not be void.");
                    errs++;
                }
                if (!type_check_expression(tc, node->var_dec.value)) {
                    err("Failed to type check expression for %.*s.",
                            (int)node->var_dec.name.length,
                            node->var_dec.name.name);
                    errs++;
                }
                if (node->var_dec.value) {
                    if (is_untyped(node->var_dec.value)) {
                        node->var_dec.value->type.type =
                            node->var_dec.type;
                        node->var_dec.value->type.state = TsOk;
                    }
                    if (!type_cmp(var_type,
                                node->var_dec.value->type.type)) {
                        err("type cmp failed in var dec");
                        errs++;
                    }
                }
                if (errs == 0) {
                    node->type.state = TsOk;
                    node->type.type = var_type;
                } else {
                    node->type.state = TsFailed;
                    node->type.type = NULL;
                }
                return errs;
            } break;
        case NodeFnDec:
            {
                // create function type checker for return
                struct TypeChecker* fn_tc = tc_fn(tc,
                        node->fn_dec.body->block.ss, &node->symbol.fn);
                int errs = 0;
                errs += type_check_node(fn_tc, node->fn_dec.body);
                if (errs == 0) {
                    node->type.state = TsOk;
                    node->type.type = node->fn_dec.return_type;
                    break;
                }
                node->type.state = TsFailed;
                node->type.type = NULL;
                free(fn_tc);
                return 0;
            } break;
        case NodeBlock:
            {
                size_t errs = 0;
                for (size_t i = 0; i < node->block.nodes_count; i++) {
                    errs += type_check_node(tc, node->block.nodes[i]);
                }
                node->type.type = NULL;
                node->type.state = TsOk;
                return errs == 0;
            } break;
        case NodeRet:
            {
                if (!tc->fn) {
                    err("no function detected, not a function or what not.");
                    return 0;
                }
                if (!type_check_node(tc, node->ret)) {
                    err("failed to type check return expression.");
                    return 0;
                }
                info("Node ret val type:"); 
                print_type(node->ret->type.type, 10);
                fflush(stdout);
                if (is_untyped(node->ret)) { // handle untyped
                    if (!is_numeric(tc->fn->return_type)) {
                        err("can not return untyped values in fucntions "
                                " that return non-numeric values.");
                        return 0;
                    }
                    // set to fucntion return values as it's numeric
                    node->ret->type.type = tc->fn->return_type;
                    node->ret->type.state = TsOk;
                }
                if (!type_cmp(node->ret->type.type, tc->fn->return_type)) {
                    err("incompatible/invalid type between return statement "
                            "and return type.");
                    print_type(node->ret->type.type, 10);
                    printf(" | ");
                    print_type(tc->fn->return_type, 10);
                    printf("\n");
                    fflush(stdout);
                    assert(0&&"incompatible/invalid type between return "
                            "statement and return type.");
                    return 0;
                }
                node->type.type = tc->fn->return_type;
                node->type.state = TsOk;
            } break;
        case NodeVar:
            {
                Variable const* v = ss_get_variable(tc->ss,node->var.name);
                if (!v) {
                    err("Failed to retrieve variable.");
                    node->type.type = NULL;
                    node->type.state = TsFailed;
                    return 0;
                }
                node->type.type = v->type;
                node->type.state = TsOk;
            } break;
        case NodeFnCall:
            {
                Function const* fn = ss_get_fn(tc->ss, node->fn_call.fn_name);
                if (!fn) {
                    err("Function not declared."); return 0;
                }
                int errs = 0;
                if (node->fn_call.args_count != fn->args_count) {
                    err("expected %zu arguments, but got %zu.",
                            fn->args_count, node->fn_call.args_count);
                    errs++;
                }
                // pick smaller one
                size_t args_count = fn->args_count < node->fn_call.args_count?
                    fn->args_count : node->fn_call.args_count;
                Argument* fn_args = fn->args;
                Node** call_args = node->fn_call.args;
                for (size_t i = 0; i < args_count; i++) {
                    if (!type_check_node(tc, call_args[i])) {
                        err("Failed to typecheck argument %zu.", i);
                        errs++;
                    } else {
                        if (type_cmp(call_args[i]->type.type,
                                    fn_args[i].type)) {
                            continue; // ok. next one
                        // else if arg is untyped handle it.
                        } else if (is_untyped(call_args[i])) {
                            // if they're both numeric then set untyped to arg
                            if (is_numeric(call_args[i]->type.type)
                                    && is_numeric(fn_args[i].type)) {
                                call_args[i]->type.type = fn_args[i].type;
                                call_args[i]->type.state = TsOk;
                            }
                            // todo: handle untyped struct etc.
                            TODO("Other forms of untyped not handled yet."
                                    " only numbers.");
                            call_args[i]->type.type = NULL;
                            call_args[i]->type.state = TsFailed;
                            errs++;
                        } else {
                            err("Invalid arg."); errs++;
                            call_args[i]->type.type = NULL;
                            call_args[i]->type.state = TsFailed;
                        }
                    }
                }
                if (errs == 0) {
                    node->type.type = fn->return_type;
                    node->type.state = TsOk;
                } else {
                    node->type.type = NULL;
                    node->type.state = TsFailed;
                }
                return errs == 0;
            } break;
        case NodeUnary:
        case NodeBinOp:
        case NodeCast:
        case NodeNumLit:
            return type_check_expression(tc, node);
        default: err("unhandled/invalid node %d", node->kind);
                 assert(0); return 0;
    }
    return 1
        && make_sure_everythings_ok_With_tc(tc, node) == 0;
}
