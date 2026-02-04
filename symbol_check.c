#include "lexer.h"
#include "parser.h"
#include "print.h"
#include "utils.h"
#include <stddef.h>
#include <stdio.h>

int determinate_type(SymbolStore* ss, Type* _type) {
    if (!_type) {
        err("Type is null.");
        return 0;
    }
    // get type of pointer or array
    Type* actual_type = get_lowest_type(_type);

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
    *actual_type = *t; // copy symbol to type
    return 1;
}

// returns 1 on succeess
int check_node_symbol(ParserCtx* pctx, SymbolStore* ss, Node* node) {
    switch (node->kind) {
        case NodeVarDec: {
			dbg("Node Var dec");
            Variable v;
            // determinate type
			// alawys use reference to node so that node is valid as well
            if (!determinate_type(ss, node->var_dec.type)) {
                err("Failed to determinate type for %.*s.",
						(int)v.name.length, v.name.name);
                return 0;
            }
            v.name = node->var_dec.name;
            v.type = node->var_dec.type;

            Type* _t = get_lowest_type(v.type);
            
            // check and add variable name
            if (!ss_new_var(ss,v)) {
                err("Failed to create symbol var: \"%.*s\".",
                    (int)v.name.length,v.name.name);
                if (ss_sym_exists(ss, v.name) != SymNone) {
                    err_sym_exists(v.name);
                } else if (ss_sym_exists(ss, v.type->name) != SymType) {
                    err("type %.*s does not exist.", (int)v.type->name.length,
                        v.type->name.name);
                } else {
                    err("no clue why.");
                }
                return 0;
            }
            char tbuf[100];
			memset(tbuf, 0, 100);
            if (node->var_dec.value)
                if (!check_node_symbol(pctx, ss, node->var_dec.value)) {
                    err("Invalid symbol in variable assignment.");
                    return 0;
                };

            // check it was created
            Variable* ss_var = ss_get_variable(ss, v.name);
            if (!ss_var) {
                err("Couldn't retrieve variable.");
                return 0;
            }
            // set symbol
            node->symbol.sym_type = SymVar;
            node->symbol.var = *ss_var;
            dbg("New var: \"%.*s\" of type \"%s\"",
                (int)v.name.length,v.name.name, tbuf);
        } break;
        case NodeFnDec: {
			dbg("Node Fn dec");
            // see if it exists first, no need to do aldat later
            if (ss_sym_exists(ss, node->fn_dec.name)) {
                err_sym_exists(node->fn_dec.name);
                return 0;
            }

            if (!node->fn_dec.return_type) { // make sure it has return type
                err("Fn ret type is null"); // must be void if not specified
                return 0;
            }
            // check return type
            if (!determinate_type(ss, node->fn_dec.return_type)) {
                err("Failed to determinate function return type");
                return 0;
            }

            Function* fn = &node->fn_dec; // nodes persist

            if (node->fn_dec.args_count > 0) {
                if (!node->fn_dec.args) {
                    err("Failed to allocate args.");
                    return 0;
                }
                // check arguments
                for (size_t i = 0; i < node->fn_dec.args_count; i++) {
                    Argument arg = node->fn_dec.args[i];
                    if (!determinate_type(ss, arg.type)) {
                        err("failed to determinate type for fn dec arg %zu.",
                                i);
                    }
                }
            }

            // create function symbol before checing block. for recursion.
            if (!ss_new_fn(ss, node->fn_dec)) {
                err("Failed to create symbol fn: \"%.*s\".",
                    (int)fn->name.length,fn->name.name);
                if (ss_sym_exists(ss, fn->name) != SymNone) {
                    err_sym_exists(fn->name);
                } else if (
                    ss_sym_exists(ss, fn->return_type->name) != SymType) {
                    err("type %.*s does not exist.",
                        (int)fn->return_type->name.length,
                        fn->return_type->name.name);
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
				Argument arg = node->fn_dec.args[i];
				Variable v;
				if (!arg.type) {
					err("Missing type data for argument %zu.", i);
					return 0;
				}
				// use arg nodetype
				if (!determinate_type(fn_ss, arg.type)) {
					err("Invalid type for arg %zu.", i);
					return 0;
				}
				v.type = arg.type;
				v.name = arg.name;
				if (!ss_new_var(fn_ss, v)) { // create arguments?? TODO:
                                             // change to
                                             // arg later
					err("Failed to create argument %zu,", i);
					return 0;
				}
			}

            if (!check_node_symbol(pctx, fn_ss, node->fn_dec.body)) {
                err("invalid symbol(s) in block.");
                return 0; // keep declared function
            }
            // make sure it exists
            // check it was created
            Function* ss_fn = ss_get_fn(ss, fn->name);
			if (ss_fn) {// set Fucntion symbol symbol store as blocks symbol
				// ss_fn->ss = node->fn_dec.body->block.ss;
            } else {
				err("Function %.*s not created",
						(int)fn->name.length, fn->name.name);
                info("\tgot fn %zu", ss_fn);
                return 0;
			}
            // set symbol
            node->symbol.sym_type = SymFn;
            node->symbol.fn = *ss_fn;
            dbg("New fn : \"%.*s\".",
                 (int)fn->name.length,fn->name.name);
        } break;
        case NodeBinOp: {
			dbg("Node Binop");
            int res = 0;
            if (!check_node_symbol(pctx, ss, node->binop.left)) {
                warn("Invalid symbol in binop left node");
                res++;
            };
            if (!check_node_symbol(pctx, ss, node->binop.right)) {
                warn("Invalid symbol in binop right node");
                res++;
            };
            if (res > 1) return 0;
        } break;
        case NodeUnary: {
			dbg("Node Unary");
            if (!check_node_symbol(pctx, ss, node->unary.target)) {
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
                    info("\tSymbol %s is a not a varibable", buf);
                } else {
                    info("\tSymbol %s does not exist.", buf);
                }
                return 0;
            };
            // set node var to copy of symbol store
            Variable* v = ss_get_variable(ss, node->var.name);
            if (!v) {
                err("variable does not exist");
                print_name(&node->var.name);
                fflush(stdout);
                assert(0);
                return 0;
            }
            node->var = *v;
            node->symbol.sym_type = SymVar;
            node->symbol.var = node->var;
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
                    info("\tSymbol %s is not a function.", buf);
                } else {
                    info("\tSymbol %s does not exist.", buf);
                }
                return 0;
            };
            // check args
            int errs = 0;
            for (size_t i = 0; i < node->fn_call.args_count; i++) {
                if (!check_node_symbol(pctx, ss, node->fn_call.args[i])) {
                    warn("\tInvalid symbol in argument %zu.", i);
                    errs++;
                }
            }
            Function* fn = ss_get_fn(ss, node->fn_call.fn_name);
            if (!fn) {
                err("fn does not exist");
                print_name(&node->fn_call.fn_name);
                fflush(stdout);
                assert(0);
                return 0;
            }
			// dbg("\terrs: %d", errs);
            if (errs > 0) return 0;
            node->symbol.sym_type = SymFn;
            node->symbol.fn = *fn;
        } break;
        // TODO: finish
        case NodeIfElse: {
			dbg("Node if else");
			int errs = 0;
			if (!node->if_else_con.base_condition) {
				err("Missing condition.");
				return 0;
			}
			if (!check_node_symbol(pctx, ss, node->if_else_con.base_condition)) {
				err("Invalid symbol in if condition.");
			}
			if (!node->if_else_con.base_block) {
				err("Missing block.");
				return 0;
			}
			if (!check_node_symbol(pctx, ss, node->if_else_con.base_block)) {
				err("Invalid symbol in if block.");
			}

			for (size_t i = 0; i < node->if_else_con.count; i++) {
				if (!node->if_else_con.alternate_conditions[i]) {
					err("Missing alternate condition %zu.", i);
					return 0; // is needed
				}
				if (!check_node_symbol(pctx, ss, node->if_else_con
							.alternate_conditions[i])) {
					err("Invalid symbol in alternate if condition %zu.", i);
					errs++;
				}
				if (!node->if_else_con.alternate_blocks[i]) {
					err("Missing alternate condition %zu block.", i);
					return 0; // is needed
				}
				if (!check_node_symbol(pctx, ss, node->if_else_con
							.alternate_blocks[i])) {
					err("Invalid symbol in alternate if block %zu.", i);
					errs++;
				}
			}

			if (node->if_else_con.else_block) {
				if (!check_node_symbol(pctx, ss, node->if_else_con.else_block)) {
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
                if (!check_node_symbol(pctx, new_ss, node->block.nodes[i])) {
                    err("invalid symbol in block expression.");
                    errs++;
                }
            }
            node->block.ss = new_ss;
			// dbg("\terrs: %d", errs);
            return errs > 0 ? 0 : 1;
        }; break;
        case NodeRet: {
			dbg("Node Return");
			if (!check_node_symbol(pctx, ss, node->ret)) {
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
			if (!check_node_symbol(pctx, ss, node->conditional.condition)) {
				err("invalid symbol in condition expression.");
				return 0;
			}
			if (!node->conditional.left_true) {
				err("Missing left expression.");
				return 0;
			}
			if (!check_node_symbol(pctx, ss, node->conditional.left_true)) {
				err("invalid symbol in left expression.");
				return 0;
			}
			if (!node->conditional.right_false) {
				err("Missing right expression.");
				return 0;
			}
			if (!check_node_symbol(pctx, ss, node->conditional.right_false)) {
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
			if (!check_node_symbol(pctx, ss,node->index.term)) {
				err("Invalid symbol in index term.");
				errs++;
			}
			if (!check_node_symbol(pctx, ss,node->index.index_expression)) {
				err("Invalid symbol in index expression.");
				errs++;
			}
			return errs > 0 ? 0 : 1;
        } break;
        case NodeNumLit: // sure it exists
            {
                int has_dot = 0;
                for (size_t i = 0; i < node->number.str_repr.length; i++) {
                    if (node->number.str_repr.name[i] == '.') has_dot = 1;
                }
                if (has_dot)
                    node->number.type = ss_get_type(ss, cstr_to_name("f32"));
                else
                    node->number.type = ss_get_type(ss, cstr_to_name("i32"));
                if (!node->number.type) {
                    err("Failed to get type from ss?");
                    return 0;
                }
            }
            break;
        case NodePrintString: {
            char _buf[1024];
            memset(_buf, 0, 1024);
            memcpy(_buf, node->print_string.string.name,
                    node->print_string.string.length);
            _cmptime_log_caller(_buf);
            fflush(stdout); }
            break;
        case NodeCast:
            {
                if (!determinate_type(ss, node->cast.to)) {
                    err("invalid type in cast expression.");
                    return 0;
                }
                return check_node_symbol(pctx, ss, node->cast.expr);
            } break;
        case NodeStructDec:
            {
                Name name = node->struct_dec.name;
                if (ss_sym_exists(ss, name)) {
                    err("symbol %.*s exist.",
                            (int)name.length, name.name);
                    return 0;
                }
                Type t;
                t.type = tt_struct;
                t.name = name;
                t.struct_data.fields = node->struct_dec.fields;
                t.struct_data.fields_count = node->struct_dec.fields_count;
                dbg("create struct %.*s.",
                        (int)name.length, name.name);
                // for names of fields
                SymbolStore* fields_ss = ss_new(NULL);
                if (!fields_ss) {
                    panic("Failed to create fields symbol store.");
                    return 0;
                }
                // check no duplicate fields and types
                for (size_t i = 0; i < t.struct_data.fields_count; i++) {
                    if (!t.struct_data.fields[i]) {
                        panic("Field %zu is null.", i); return 0;
                        return 0;
                    }
                    Field f;
                    info("%zu",t.struct_data.fields[i]); 
                    f.name = t.struct_data.fields[i]->field.name;
                    f.type = t.struct_data.fields[i]->field.type;
                    if (f.name.name == 0 || f.name.length == 0){
                        panic("struct field name is 0.");
                        return 0;
                    }
                    if (!f.type) {
                        panic("Fieldn't doesn't have a type.");
                        return 0;
                    }
                    // get lowest type if it's a pointer/array
                    Type* lowest_type = get_lowest_type(f.type);
                    if (!lowest_type) {
                        panic("Doesn't have lowest type.");
                        return 0;
                    }
                    Type* exists = ss_get_type(ss, lowest_type->name);
        
                    if (!exists) {
                        err("Field type does not exist.");
                        return 0;
                    } else {
                        // set lowest type to gotten type
                        *lowest_type = *exists;
                    }
                    if (!ss_new_field(ss, f)) {
                        err("failed to create field for ss.");
                        if (ss_sym_exists(ss, f.name)) {
                            info("Duplicate field %.*s.",
                                    (int)f.name.length, f.name.name);
                        }
                        return 0;
                    }
                }
                free(fields_ss->syms); // free cus why not
                free(fields_ss);
                if (!ss_new_type(ss, t)) {
                    panic("Failed to create type struct %.*s.",
                            (int)name.length, name.name);
                    return 0;
                }
                dbg("Created struct.");
            } break;
		case NodeField:
			{
				if (!check_node_symbol(pctx,ss, node->field.target)) {
					panic("Failed to symbol check field access.");
					return 0;
				}
			} break;
        default:
            err("Invalid node type in name check. Node %s.", node_type_to_string(node->kind));
            if (node->token.type != TokenNone) {
                info("\tToken data: %s.", get_token_data(node->token));
            }
            assert(0);
            break;
    }
    return 1;
}


int is_valid_type(Type* t) {
    if (!t)  return (err("No type")),0;
    if (t->type == tt_to_determinate) return (err("to determinate")), 0;
    // void has size 0
    if (t->size == 0 && t->type != tt_void && t->type != tt_ptr) return (err("size 0")), 0;;
    if (t->type == tt_ptr && t->ptr == 0) return (err("no ptr")), 0;;;
    return 1;
}

int symbols_check(Node* node) {
	dbg("symbol checking node %s...", node_type_to_string(node->kind));
    switch (node->kind) {
        case NodeNumLit:
            return 1; // fix in autoassign later in typecheck
        case NodeVar:
            if (!is_valid_type(node->var.type)) err("invalid var");
            return is_valid_type(node->var.type);
        case NodeVarDec:
            return is_valid_type(node->var_dec.type)
                && node->var_dec.value ? // check node value
                symbols_check(node->var_dec.value) : 1;
        case NodeFnDec:
            {
                size_t errs = 0;
                if (!is_valid_type(node->fn_dec.return_type)) errs++;
                for (size_t i = 0; i < node->fn_dec.args_count; i++) {
                    if (!is_valid_type(node->fn_dec.args[i].type)) {
                        err("invalid type in arg");
                        Type* t = node->fn_dec.args[i].type;
                        if (!t) err("t is null");
                        if (t->type == tt_to_determinate) err("T is to determinate");
                        if (t->size == 0 && t->type != tt_void) err("T size is 0");
                        if (t->type == tt_ptr && t->ptr == 0) err("T is a ptr but ptr is null");
                        info("%zu %.*s", t->name.length, (int)t->name.length, t->name.name);
                        errs++;
                    }
                }
                if (!symbols_check(node->fn_dec.body))
                    errs++;
                return errs == 0;
            }
		case NodeUnary:
			return symbols_check(node->unary.target);
		case NodeBinOp:
			{
				size_t errs = 0;
				if (!symbols_check(node->binop.left)) {
					errs++;
					err("Binop left failed.");
				}
				if (!symbols_check(node->binop.right)) {
					errs++;
					err("Binop right failed.");
				}
				return errs == 0;
			}
        case NodeFnCall:
            {
                size_t errs = 0;
                for (size_t i = 0; i < node->fn_call.args_count; i++)
                    if (!symbols_check
                            (node->fn_call.args[i])) {
                        err("invalid arg in fn call");
                        errs++;
                    }
                return errs == 0;
            }
        case NodeRet:
            return symbols_check(node->ret);
        case NodeBlock:
            {
                size_t errs = 0;
                for (size_t i= 0; i < node->block.nodes_count; i++) {
                    Node* n = node->block.nodes[i];
                    if (!symbols_check(n)) errs++;
                }
                if (errs != 0) err("failed in block");
                return errs == 0;
            }
        case NodeCast:
            {
                if (!is_valid_type(node->cast.to)) return 0;
                return symbols_check(node->cast.expr);
            } break;
		case NodeIfElse:
			{
				Node* base_con = node->if_else_con.base_condition;
				Node* base_block = node->if_else_con.base_block;
				Node** alt_cons = node->if_else_con.alternate_conditions;
				Node** alt_blocks = node->if_else_con.alternate_blocks;
				Node* else_block = node->if_else_con.else_block;
				size_t errs = 0;
				if (!symbols_check(base_con)) {
					errs++;
					err("base con failed.");
				}
				if (!symbols_check(base_block)) {
					errs++;
					err("base block failed.");
				}
				for (size_t i = 0; i < node->if_else_con.count; i++) {
					if (!symbols_check(alt_cons[i])) {
						errs++;
						err("alt con %zu failed.", i);
					}
					if (!symbols_check(alt_blocks[i])) errs++;
						err("alt block %zu failed.", i);
				}
				if (else_block) if (!symbols_check(else_block)) {
					errs++;
					err("else block failed.");
				}

				// panic("if else con");
				if (errs != 0) {
					panic("errs != 0 in if else: %zu errors.", errs);
				}
				return errs == 0;
			} break;
        // not much to check here.
        case NodeStructDec:
            {
				// struct dec is a type. check if fields have a valid type.
				for (size_t i = 0; i < node->struct_dec.fields_count; i++) {
					Node* field = node->struct_dec.fields[i];
					if (!field) {
						panic("Field is NULL.");
						return 0;
					}
					if (!is_valid_type(field->field.type)) {
						panic("field type is invalid.");
						return 0;
					}
				}
                // TODO("implement checkign for types here.");
                return 1;
            } break;
		case NodeField:
			{
				info("Node Field");
				if (!symbols_check(node->field.target)) {
					panic("Failed to symbol check field acces.");
					return 0;
				}
				dbg("Field access ok.");
				return 1;
			} break;
		case NodeIndex:
			{
				size_t errs = 0;
				if (!symbols_check(node->index.term)) {
					err("Failed to sumbol check index term.");
					errs++;
				}
				if (!symbols_check(node->index.index_expression)) {
					err("Failed to sumbol check index index_expression.");
					errs++;
				}
				return errs == 0;
			} break;
        default: err("Invalid/unhandled node %d", node->kind);
                 assert(0);
    }
	panic("Shouldn't reach this.");
	return 0;
}

