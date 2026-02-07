#include "type_checker.h"
#include "parser.h"
#include "print.h"
#include "utils.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include "user_msgs.h"

// TODO improve
// only unsigned integer types - untyped, uxx, ptrs too ig. they are just uxx
int can_index(Node* n) {
    if (!n) {
        panic("Node is NULL.");
        return 0;
    }
    if (n->type.state == TsUntypedUnsignedInt) return 1;
    if (n->type.type) {
        if (is_unsigned(n->type.type)) {
            return 1;
        }
        print_type(n->type.type, 10);
        panic("Can't use type as index.");
    }
    panic("No type in can index.");
    return 0;
}
// TODO improve
int can_reference(Node* n) {
    // if (n->kind == NodeNumLit) return 0;
    if (is_untyped(n)) return 0; // can not reference untyped values
    return 1;
}
// TODO improve
int can_dereference(Node* target) {
    if (!type_info_is_numeric(target->type)) {
        err("Can only dereference numeric values.");
        return 0;
    }
    if (!target->type.type) {
        err("target must have type to derefefrence.");
        // should only happen when target is untyped
        if (is_untyped(target)) {
            err("Cannot dereference untyped value.");
        } else {
            panic("Type checker failed.");
        }
        return 0;
    }
    if (target->type.type->type != tt_ptr) {
        err("Can only dereference pointers.");
    }
    return 1;
}
int can_be_indexed(Node* n) {
    if (n->type.state != TsOk) {
        err("state is not ok in can_be_indexed.");
        return 0;
    }
    if (!n->type.type) {
        err("Type is null in can_be_indexed.");
        return 0;
    }
    if (can_dereference(n)) 
        return 1;
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
    // return ;
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
    // types need to match + other stuff like pointers
    if (t1->type == t2->type) {
        if (t1->type == tt_ptr) {
            return type_cmp(t1->ptr, t1->ptr);
        }
        else if (name_cmp(t1->name, t2->name)) { // else same name return true
            return 1;
        } // otherwise types don't match
        else return 0;
    }
    // if types types aren't the same then types don't match
    return 0;
}

// returns type to which to cast to (t1, t2 or null if none)
Type* can_implicit_cast(Type* t1, Type* t2) {
    if (type_cmp(t1, t2)) return t1;
    info("Implicit cast");

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

Function* tc_get_function(struct TypeChecker* tc) {
    TypeChecker* this = tc;
    while (this != NULL) {
        if (this->fn) return this->fn;
        this = this->parent; // for scopes
    }
    return NULL;
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
    dbg("exprcheck %s", node_type_to_string(node->kind));
    switch (node->kind) {
        case NodeNumLit:
            {
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
                    node->type.state = TsUntypedUnsignedInt;
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
                info("Checking binop for.");
                print_two_types(node->binop.left->type.type,
                        node->binop.right->type.type);
                fflush(stdout);
                // check if you can actually perform binop on types
                info("checking binop types %s and %s",
                        node_type_to_string(node->binop.right->kind),
                        node_type_to_string(node->binop.left->kind)
                    );
                if (!can_binop(node->binop.left->type)
                        || !can_binop(node->binop.right->type)) {
                    print_two_types(node->binop.left->type.type,
                            node->binop.right->type.type);
                    fflush(stdout);
                    panic("Cannot binop types %s and %s",
                            node_type_to_string(node->binop.right->kind),
                            node_type_to_string(node->binop.left->kind)
                            );
                    return 0;
                }
                if (!handle_binop_untyped(&node->binop.left->type,
                        &node->binop.right->type)) {
                    err("Failed to handle untyped term in binop");
                    return 0;
                }
                // if both are untyped
                if (is_untyped(node->binop.left)
                        && is_untyped(node->binop.right)) {

                    // prioritise floats
                    if (node->binop.left->type.state == TsUntypedFloat)
                        node->type = node->binop.left->type;
                    else
                        node->type = node->binop.right->type;
                    break;
                }
                if (!type_cmp(node->binop.left->type.type,
                            node->binop.right->type.type)) {
                    err("fuckass binop "
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
                info("Unary");
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
                            if (!can_dereference(target)) {
                                err("can not dereference target.");
                                usr_error(tc->pctx, "Can not dereference target.", target);
                                node->type.type = NULL;
                                node->type.state = TsFailed;
                                return 0;
                            }
                            if (target->type.type->type == tt_ptr) {
                                node->type.type = target->type.type->ptr;
                                node->type.state = TsOk;
                            /*} else if (is_numeric(target->type.type)) {
                                node->type.type = NULL;
                                node->type.state = TsNeedsType;*/ // ignode for noew
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
        case NodeIndex: // assignmetns/binop can
        case NodeFieldAccess:
        case NodeFnCall:
            return type_check_node(tc, node);
        default:panic("node can not be part of an expression");
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
                // print_type(node->type.type, 10);
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
                if (node->var_dec.value) {
                    if (!type_check_expression(tc, node->var_dec.value)) {
                        err("Failed to type check expression for %.*s.",
                                (int)node->var_dec.name.length,
                                node->var_dec.name.name);
                        errs++;
                        return 0;
                    }
                    // change once vardec has implicit types
                    if (is_untyped(node->var_dec.value)) {
                        node->var_dec.value->type.type =
                            node->var_dec.type;
                        node->var_dec.value->type.state = TsOk;
                    }
                    info("Checking types.");
                    if (!type_cmp(var_type,
                                node->var_dec.value->type.type)) {
                        err("type cmp failed in var dec");
                        panic("Failed type cmp");
                        errs++;
                    }
                }
                if (errs == 0) {
                    node->type.state = TsOk;
                    node->type.type = var_type;
                    return 1;
                } else {
                    err("Errs not 0 in vardec wilted flower.");
                    node->type.state = TsFailed;
                    node->type.type = NULL;
                    return 0;
                }
            } break;
        case NodeFnDec:
            {
                // create function type checker for return
                TypeChecker* fn_tc = tc_fn(tc,
                        node->fn_dec.body->block.ss, &node->symbol.fn);
                if (!type_check_node(fn_tc, node->fn_dec.body)) {
                    free(fn_tc);
                    err("Failed typechekc of fn body.");
                    node->type.state = TsFailed;
                    node->type.type = NULL;
                    return 0;
                }
                free(fn_tc);
                node->type.state = TsOk;
                node->type.type = node->fn_dec.return_type;
            } break;
        case NodeBlock:
            {

                TypeChecker* block_tc = new_scope(tc, node->block.ss);
                if (!block_tc) {
                    panic("Failed to allocate type chekcer.");
                    return 0;
                }
                size_t errs = 0;
                for (size_t i = 0; i < node->block.nodes_count; i++) {
                    if (!type_check_node(block_tc, node->block.nodes[i])) {
                        panic("error in node %s.", node_type_to_string(
                                    node->block.nodes[i]->kind));
                        errs++;
                    }
                }
                free(block_tc);
                node->type.type = NULL;
                node->type.state = TsOk;
                dbg("end of block errs: %zu.", errs);
                return errs == 0;
            } break;
        case NodeRet:
            {
                Function* fn = tc_get_function(tc);
                if (!fn) {
                    err("no function detected, not a function or what not.");
                    return 0;
                }
                if (!type_check_node(tc, node->ret)) {
                    err("failed to type check return expression.");
                    return 0;
                }
                if (is_untyped(node->ret)) { // handle untyped
                    dbg("Is numeric");
                    if (!is_numeric(fn->return_type)) {
                        err("can not return untyped values in fucntions "
                                " that return non-numeric values.");
                        return 0;
                    }
                    // set to fucntion return values as it's numeric
                    node->ret->type.type = fn->return_type;
                    node->ret->type.state = TsOk;
                }
                if (!type_cmp(node->ret->type.type, fn->return_type)) {
                    usr_error(tc->pctx, "incompatible/invalid type between return statement "
                            "and return type.", node->ret);
                    info("Token %s", get_token_data(node->ret->token));
                    panic("incompatible/invalid type between return "
                            "statement and return type.");
                    return 0;
                }
                fflush(stdout);
                node->type.type = fn->return_type;
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
                    usr_error(tc->pctx,
                            "Incorrect number of arguments supplied.", node);
                    printf(TAB4"expected %zu arguments, but got %zu.",
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
                            usr_error(tc->pctx, "Invalid argument.", call_args[i]);
                            char name1[100];
                            char name2[100];
                            type_to_human_str(name1, 100, fn_args[i].type);
                            type_to_human_str(name2, 100, call_args[i]->type.type);
                            printf(TAB4"Expected %s, got %s.\n",
                                    name1, name2);

                            errs++;
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
        case NodeIfElse:
            {
                Node* base_con = node->if_else_con.base_condition;
                Node* base_block = node->if_else_con.base_block;
                Node** alt_cons = node->if_else_con.alternate_conditions;
                Node** alt_blocks = node->if_else_con.alternate_blocks;
                Node* else_block = node->if_else_con.else_block;
                size_t errs = 0;
                if (!type_check_node(tc, base_con)) {
                    errs++;
                    err("base con failed type check typecheck.");
                }
                if (!type_check_node(tc, base_block)) {
                    errs++;
                    err("base block failed type check typecheck.");
                }
                for (size_t i = 0; i < node->if_else_con.count; i++) {
                    if (!type_check_node(tc, alt_cons[i])) {
                        errs++;
                        err("alt con %zu failed typecheck.", i);
                    }
                    if (!type_check_node(tc, alt_blocks[i])) errs++;
                        err("alt block %zu failed typecheck.", i);
                }
                if (else_block) if (!type_check_node(tc, else_block)) {
                    errs++;
                    err("else block failed typecheck.");
                }
                node->type.type = NULL;
                node->type.state = TsOk;
                if (errs != 0) {
                    panic("errs != 0 in if else typecheck: %zu errors.", errs);
                    node->type.state = TsFailed;
                }
                return errs == 0;
            } break;
        case NodeStructDec:
            {
                // nothing to type check
                return 1;
                panic("Todo.");
            };
        case NodeIndex:
            {
                int errs = 0;
                if (!node->index.target) {
                    panic("No target in index");
                    errs++;
                } else{
                    if (!type_check_node(tc, node->index.target)) {
                        err("Failed to type check index target.");
                        errs++;
                    } else {
                        if (!can_be_indexed(node->index.target)) {
                            err("Node target can't be indexed. %s.",
                                    node_type_to_string(
                                        node->index.target->kind));
                            print_type(node->index.target->type.type, 10);
                            errs++;
                        }
                    }
                }
                if (!node->index.index_expression) {
                    panic("No index in index");
                    errs++;
                } else {
                    if (!type_check_node(tc, node->index.index_expression)) {
                        err("Failed to type check index index.");
                        errs++;
                    } else {
                        if (!can_index(node->index.index_expression)) {
                            err("index index can not index. Node %s",
                                    node_type_to_string
                                    (node->index.index_expression->kind));
                            errs++;
                        }
                    }
                }
                // on success, update type, bruh
                if (errs == 0) {
                    // set to target type ptr type for now.
                    // TODO improve,make like a get index type fn
                    node->type = node->index.target->type;
                    node->type.type = node->type.type->ptr; // set to ptr type
                }
                return errs == 0;
            } break;
        case NodeFieldAccess:
            {
                int errs = 0;
                Node* target = node->field_access.target;
                if (!target) {
                    panic("No target in field.");
                    return 0;
                }
                if (!is_valid_name(node->field_access.name)) {
                    panic("Invalid name in field.");
                    return 0;
                }
                if (!type_check_node(tc, target)) {
                    err("Failed to typecheck field target.");
                    return 0;
                }
                /* if (!target->type.state = TsOk) {
                    err("Cannot acces fields of untyped structs.");
                    return 0;
                } */ // not sure. prob make a "can_access_fields" func
                if (target->type.type->type != tt_struct) {
                    err("target type type(TypeType)"
                            " must be struct for field access.");
                    print_type(target->type.type, 10);
                    return 0;
                }
                Name access_name = node->field_access.name;
                Type* type  = target->type.type;
                // check if feild_name is in fields
                Field field;
                for (size_t i = 0; i < type->struct_data.fields_count; i++) {
                    Field f = type->struct_data.fields[i];
                    // if found
                    if (name_cmp(access_name, f.name)) {
                        node->type.type = f.type;
                        node->type.state =TsOk;
                        field = f;
                        // return 1;
                    }
                }
                if (errs == 0) {
                    node->type.type = field.type;
                    node->type.state = TsOk;
                    return 1;
                }
                panic("%zu errors. Couldn't find field in struct/field"
                        "%.*s doesn't exist in %.*s.", errs,
                        (int)access_name.length, access_name.name,
                        (int)target->type.type->name.length,
                        target->type.type->name.name);
                node->type.type = NULL;
                node->type.state = TsFailed;
                return 0;
                return errs == 0;
            } break;
        case NodeUnary:
        case NodeBinOp:
        case NodeCast:
        case NodeNumLit:
            return type_check_expression(tc, node);
        default: err("unhandled/invalid node %s (%d)",
                         node_type_to_string(node->kind),
                         node->kind);
                 assert(0); return 0;
    }
    return 1
        && make_sure_everythings_ok_With_tc(tc, node) == 0;
}
