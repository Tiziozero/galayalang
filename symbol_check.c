#include "lexer.h"
#include "parser.h"
#include "utils.h"
#include <stdio.h>

int determinate_type(SymbolStore* ss, Type* _type) {
    // get type of pointer or array
    Type* actual_type = _type;

    while (actual_type->type == tt_ptr || actual_type->type == tt_array ) {
        if (actual_type->type == tt_ptr && actual_type->ptr != NULL)
            actual_type = actual_type->ptr;
        else if (actual_type->type == tt_array
                && actual_type->static_array.type != NULL) {
            actual_type = actual_type->static_array.type;
    } else {
            err("invalid type");
            return 0;
        }
    }
    // check if it exists
    SymbolType st = ss_sym_exists(ss, actual_type->name);
    if (st == SymNone) {
        err("Symbol %.*s does not exist",
            (int)actual_type->name.length, actual_type->name.name);
        return 0;
    }
    // make sure it's a type
    if (st != SymType) {
        err("Symbol is not a type");
        return 0;
    }
    // get reference to that type
    Type* t = ss_get_type(ss, actual_type->name);
    if (!t) {
        err("got null for %.*s.",
            (int)actual_type->name.length, actual_type->name.name);
        return 0;
    }
    // assign type parsed to ptr to type in symbol table
    *actual_type = *t;
    return 1;
}

// returns 1 on succeess
int check_node_symbol(SymbolStore* ss, Node* node) {
    switch (node->type) {
        case NodeVarDec: {
			dbg("Node Var dec");
            Variable v;
            // determinate type
			// alawys use reference to node so that node is valid as well
            if (!determinate_type(ss, &node->var_dec.type)) {
                err("Failed to determinate type for %.*s.",
						(int)v.name.length, v.name.name);
                return 0;
            }
            v.name = node->var_dec.name;
            v.type = node->var_dec.type;

            Type _t = v.type;
            while(_t.type == tt_array || _t.type == tt_ptr) {
                if (_t.type == tt_array) {
                    _t = *_t.static_array.type;
                }
                if (_t.type == tt_ptr) {
                    _t = *_t.ptr;
                }
            }
            // check and add variable name
            if (!ss_new_var(ss,v)) {
                err("Failed to create symbol var: \"%.*s\".",
                    (int)v.name.length,v.name.name);
                if (ss_sym_exists(ss, v.name) != SymNone) {
                    err_sym_exists(v.name);
                } else if (ss_sym_exists(ss, v.type.name) != SymType) {
                    err("type %.*s does not exist.", (int)v.type.name.length,
                        v.type.name.name);
                } else {
                    err("no clue why.");
                }
                return 0;
            }
            char tbuf[100];
			memset(tbuf, 0, 100);
			print_type_to_buffer(tbuf, &v.type);
            dbg("New var: \"%.*s\" of type \"%s\"",
                (int)v.name.length,v.name.name, tbuf);
            if (node->var_dec.value)
                if (!check_node_symbol(ss, node->var_dec.value)) {
                    err("Invalid symbol in variable assignment.");
                    return 0;
                };
        } break;
        case NodeFnDec: {
			dbg("Node Fn dec");
            // see if it exists first, no need to do aldat later
            Name name = node->fn_dec.name;
            if (ss_sym_exists(ss, name)) {
                err_sym_exists(name);
                return 0;
            }

            if (!node->fn_dec.return_type) { // make sure it has return type
                err("Fn ret type is null");
                return 0;
            }
            // check return type
            if (!determinate_type(ss, &node->fn_dec.return_type->type_data)) {
                err("Failed to determinate function return type");
                return 0;
            }

            Function fn = {0};
            memset(&fn, 0, sizeof(Function)); // init to 0
            fn.name = node->fn_dec.name;
            fn.args = 0;
            fn.args_count = 0;
            fn.args_capacity = 0;
            fn.return_type = node->fn_dec.return_type->type_data;

            // create function symbol before checing block. for recursion.
            if (!ss_new_fn(ss, fn)) {
                err("Failed to create symbol fn: \"%.*s\".",
                    (int)fn.name.length,fn.name.name);
                if (ss_sym_exists(ss, fn.name) != SymNone) {
                    err_sym_exists(fn.name);
                } else if (
                    ss_sym_exists(ss, fn.return_type.name) != SymType) {
                    err("type %.*s does not exist.",
                        (int)fn.return_type.name.length,
                        fn.return_type.name.name);
                } else {
                    err("no clue why.");
                }
                return 0;
            }

            // make sure it has a body (for now);
            if (!node->fn_dec.body) {
                err("Function body is null.");
                return 0;
            }
			// block symbol store will reference this (for args)
			// and this will reference current ss, so that this one
			// can still be accessed later on when type checking via the block
			SymbolStore* fn_ss = ss_new(ss);
			if (!fn_ss) {
				err("Failed to create function symbol store.");
				return 0;
			}
			// for each arg add to fn_ss
			for (size_t i = 0; i < node->fn_dec.args_count; i++) {
				Node* arg = node->fn_dec.args[i];
				if (!arg) {
					err("Invalid arg %zu.", i);
					continue;
				}
				if (arg->type != NodeArg) {
					err("Arg node %zu is not an arg.", i);
					return 0;
				}
				Variable v;
				if (!arg->arg.type) {
					err("Missing type data for argument %zu.", i);
					return 0;
				}
				// use arg nodetype
				if (!determinate_type(fn_ss, &arg->arg.type->type_data)) {
					err("Invalid type for arg %zu.", i);
					return 0;
				}
				v.type = arg->arg.type->type_data;
				v.name = arg->arg.name;

				if (!ss_new_var(fn_ss, v)) {
					err("Failed to create argument %zu,", i);
					return 0;
				}
			}

            if (!check_node_symbol(fn_ss, node->fn_dec.body)) {
                err("invalid symbol(s) in block.");
                return 0; // keep declared function
            }
            Function* _got_fn = ss_get_fn(ss, fn.name);
			if (_got_fn) // set Fucntion symbol symbol store as blocks symbol
				ss_get_fn(ss, fn.name)->ss = node->fn_dec.body->block.ss;
			else {
				err("Function %.*s not created",
						(int)fn.name.length, fn.name.name);
                info("\t%zu %zu", _got_fn, _got_fn->ss);
			}
            dbg("New fn : \"%.*s\".",
                 (int)fn.name.length,fn.name.name);
        } break;
        case NodeBinOp: {
			dbg("Node Binop");
            int res = 0;
            if (!check_node_symbol(ss, node->binop.left)) {
                warn("Invalid symbol in binop left node");
                res++;
            };
            if (!check_node_symbol(ss, node->binop.right)) {
                warn("Invalid symbol in binop right node");
                res++;
            };
            if (res > 1) return 0;
        } break;
        case NodeUnary: {
			dbg("Node Unary");
            if (!check_node_symbol(ss, node->unary.target)) {
                err("Invalid symbol in unary op node");
                return 0;
            };
        } break;
        case NodeVar: {
			dbg("Node Var");
            char buf[100];
            print_name_to_buf(buf, 100, node->var.name);

            SymbolType st;
            if ((st = ss_sym_exists(ss, node->var.name)) != SymVar) {
                err("variable %s doesn't exist.", buf);
                if (node->token.type!= TokenEOF) {
                    info("\tin line %zu:%zu.", node->token.line,
                         node->token.col);
                } else {
                    info("Can't tell token position.");
                }
                if (st != SymNone) {
                    info("\tSymbol %s is a %s", buf, get_sym_type(st));
                } else {
                    info("\tSymbol %s does not exist.", buf);
                }
                return 0;
            };
        } break;
        case NodeFnCall: {
			dbg("Node Fn Call");
            char buf[100];
            print_name_to_buf(buf, 100, node->fn_call.fn_name);

            SymbolType st;
            if ((st = ss_sym_exists(ss, node->fn_call.fn_name)) != SymFn) {
                err("fn_call %s doesn't exist.", buf);
                if (node->token.type!= TokenEOF) {
                    info("\tin line %zu:%zu.", node->token.line,
                         node->token.col);
                } else {
                    info("Can't tell token position.");
                }
                if (st != SymNone) {
                    info("\tSymbol %s is a %s", buf, get_sym_type(st));
                } else {
                    info("\tSymbol %s does not exist.", buf);
                }
                return 0;
            };
            // check args
            int errs = 0;
            for (size_t i = 0; i < node->fn_call.args_count; i++) {
                if (!check_node_symbol(ss, node->fn_call.args[i])) {
                    warn("\tInvalid symbol in argument %zu.", i);
                    errs++;
                }
            }
			// dbg("\terrs: %d", errs);
            if (errs > 0) return 0;
        } break;
        // TODO: finish
        case NodeIfElse: {
			dbg("Node if else");
			int errs = 0;
			if (!node->if_else_con.base_condition) {
				err("Missing condition.");
				return 0;
			}
			if (!check_node_symbol(ss, node->if_else_con.base_condition)) {
				err("Invalid symbol in if condition.");
			}
			if (!node->if_else_con.base_block) {
				err("Missing block.");
				return 0;
			}
			if (!check_node_symbol(ss, node->if_else_con.base_block)) {
				err("Invalid symbol in if block.");
			}

			for (size_t i = 0; i < node->if_else_con.count; i++) {
				if (!node->if_else_con.alternate_conditions[i]) {
					err("Missing alternate condition %zu.", i);
					return 0; // is needed
				}
				if (!check_node_symbol(ss, node->if_else_con
							.alternate_conditions[i])) {
					err("Invalid symbol in alternate if condition %zu.", i);
					errs++;
				}
				if (!node->if_else_con.alternate_blocks[i]) {
					err("Missing alternate condition %zu block.", i);
					return 0; // is needed
				}
				if (!check_node_symbol(ss, node->if_else_con
							.alternate_blocks[i])) {
					err("Invalid symbol in alternate if block %zu.", i);
					errs++;
				}
			}

			if (node->if_else_con.else_block) {
				if (!check_node_symbol(ss, node->if_else_con.else_block)) {
					err("Invalid symbol in else block.");
					errs++;
				}
			}
			// dbg("\terrs: %d", errs);
			return errs > 0 ? 0 : 1;
        } break;
        case NodeBlock: {
			dbg("Node Block");
            SymbolStore* new_ss = ss_new(ss);
            if (!new_ss) {
                err("Failed to create symbol store.");
                return 0;
            }
            int errs = 0;
            for (size_t i = 0; i < node->block.nodes_count; i++) {
                if (!check_node_symbol(new_ss, node->block.nodes[i])) {
                    err("invalid symbol in block expression. %zu",
							node_type_to_string(node->block.nodes[i]->type));
                    errs++;
                }
            }
            node->block.ss = new_ss;
			// dbg("\terrs: %d", errs);
            return errs > 0 ? 0 : 1;
        }; break;
        case NodeRet: {
			dbg("Node Return");
			if (!check_node_symbol(ss, node->ret)) {
				err("invalid symbol in return expression.");
				return 0;
			}
        }; break;
        case NodeConditional: {
			dbg("Node NodeConditional");
			if (!node->conditional.condition) {
				err("Missing condition.");
				return 0;
			}
			if (!check_node_symbol(ss, node->conditional.condition)) {
				err("invalid symbol in condition expression.");
				return 0;
			}
			if (!node->conditional.left_true) {
				err("Missing left expression.");
				return 0;
			}
			if (!check_node_symbol(ss, node->conditional.left_true)) {
				err("invalid symbol in left expression.");
				return 0;
			}
			if (!node->conditional.right_false) {
				err("Missing right expression.");
				return 0;
			}
			if (!check_node_symbol(ss, node->conditional.right_false)) {
				err("invalid symbol in right expression.");
				return 0;
			}
        }; break;
        case NodeIndex: {
			if (!node->index.term) {
				err("Missing index term???");
				return 0;
			}
			if (!node->index.index_expression) {
				err("Missing index expression.");
				return 0;
			}
			int errs = 0;
			if (!check_node_symbol(ss,node->index.term)) {
				err("Invalid symbol in index term.");
				errs++;
			}
			if (!check_node_symbol(ss,node->index.index_expression)) {
				err("Invalid symbol in index expression.");
				errs++;
			}
			return errs > 0 ? 0 : 1;
        } break;
        case NodeNumLit: // sure it exists
            if (!determinate_type(ss, &node->number.type)) {
                err("Invalid type for num literal %s.",
                        get_token_data(node->token));
                return 0;
            }
            break;
        case NodePrintString:
            char _buf[1024];
            memset(_buf, 0, 1024);
            memcpy(_buf, node->print_string.string.name,
                    node->print_string.string.length);
            _cmptime_log_caller(_buf);
            fflush(stdout);
            break;
        default:
            err("Invalid node type in name check: %s",
                node_type_to_string(node->type));
            if (node->token.type != TokenNone) {
                info("\tToken data: %s.", get_token_data(node->token));
            }
            assert(0);
            break;
    }
    return 1;
}

