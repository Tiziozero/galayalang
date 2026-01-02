#include "lexer.h"
#include "parser.h"
// this file is also a mess
const char* node_type_to_string(NodeType type) {
    switch (type) {
        case NodeNone: return "NodeNone";
        case NodeCast: return "NodeCast";
        case NodeVarDec: return "NodeVarDec";
        case NodeVar: return "NodeVar";
        case NodeField: return "NodeField";
        case NodeIndex: return "NodeIndex";
        case NodeUnary: return "NodeUnary";
        case NodeNumLit: return "NodeNumLit";
        case NodeArg: return "NodeArg";
        case NodeBinOp: return "NodeBinOp";
        case NodeFnDec: return "NodeFnDec";
        case NodeIfElse: return "NodeIfElse";
        case NodeFnCall: return "NodeFnCall";
        case NodeConditional: return "NodeConditional";
        case NodeBlock: return "NodeBlock";
        case NodeRet: return "NodeRet";
        case NodeTypeData: return "NodeTypeData";
        default:            return "<unknown node type>";
    }
}
void print_symbol(const Symbol* sym, int indent) {
    for (int i = 0; i < indent; i++) printf("  ");

    printf("- %s: ", get_sym_type(sym->sym_type));

    switch (sym->sym_type) {
        case SymVar:
            printf("name=");
            print_name(sym->var.name);
            printf(", type=");
            print_type(sym->var.type, 0);
            break;

        case SymArg:
            printf("name=");
            print_name(sym->argument.name);
            printf(", type=");
            print_type(sym->argument.type, 0);
            break;

        case SymField:
            printf("name=");
            print_name(sym->field.name);
            printf(", type=");
            print_type(sym->field.type, 0);
            break;

        case SymType:
            printf("type=");
            print_type(&sym->type, 0);
            break;

        case SymFn:
            printf("name=");
            print_name(sym->fn.name);
            printf(", returns=");
            print_type(sym->fn.return_type, 0);
            printf("\n");

            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("args (%zu):\n", sym->fn.args_count);

            for (size_t i = 0; i < sym->fn.args_count; i++) {
                for (int j = 0; j < indent + 2; j++) printf("  ");
                printf("- ");
                print_name(sym->fn.args[i].name);
                printf(": ");
                print_type(sym->fn.args[i].type, 0);
                printf("\n");
            }
            print_symbol_store(sym->fn.ss, indent + 2);
            return;

        default:
            printf("<unknown>");
            break;
    }

    printf("\n");
}
void print_symbol_store(const SymbolStore* store, int indent) {
    if (!store) return;

    for (int i = 0; i < indent; i++) printf("  ");
    printf("SymbolStore (%zu symbols)\n", store->syms_count);

    for (size_t i = 0; i < store->syms_count; i++) {
        print_symbol(&store->syms[i], indent + 1);
    }
}
// returns 1 on true
int is_cmpt_constant(Node* expr) {
    switch (expr->type) {
        case NodeBinOp: {
            if (!is_cmpt_constant(expr->binop.left)) 
                return 0;
            else if (!is_cmpt_constant(expr->binop.right)) 
                return 0;
        } break;
        case NodeNumLit: {
        } break;
        case NodeUnary: {
            if (!is_cmpt_constant(expr->unary.target)) 
                return 0;
        } break;
        case NodeVarDec:  {
            if (!expr->var_dec.value) break;
            if (!is_cmpt_constant(expr->var_dec.value)) 
                return 0;
        } break;
        default:
            err("can not determinale compile time value of expression:"
                    " %s %s.", node_type_to_string(expr->type),
                    get_token_data(expr->token)); return 0;
    }
    return 1;
}

const char* get_node_data(Node* node) {
    static char buf[128];

    if (!node) {
        snprintf(buf, sizeof(buf), "NULL");
        return buf;
    }

    switch (node->type) {
        case NodeNumLit:
            snprintf(buf, sizeof(buf),
                     "%s(%g)",
                     node_type_to_string(node->type),
                     node->number.number);
            break;

        case NodeBinOp:
            snprintf(buf, sizeof(buf),
                     "%s(%s)",
                     node_type_to_string(node->type),
                     optype_to_string(node->binop.type));
            break;

        case NodeVar:
            snprintf(buf, sizeof(buf),
                     "%s(%.*s)",
                     node_type_to_string(node->type),
                     (int)node->var.name.length,
                     node->var.name.name);
            break;

        case NodeVarDec:
            snprintf(buf, sizeof(buf),
                     "%s(%.*s)",
                     node_type_to_string(node->type),
                     (int)node->var_dec.name.length,
                     node->var_dec.name.name);
            break;

        default:
            snprintf(buf, sizeof(buf),
                     "%s",
                     node_type_to_string(node->type));
            break;
    }

    return buf;
}
const char* get_sym_type(SymbolType st) {
    switch (st) {
        case SymVar: return "Variable (SymVar)";
        case SymFn: return "Function (SymFn)";
        case SymArg: return "Argument (SymArg)";
        case SymType: return "Type (SymType)";
        case SymField: return "Field (SymField)";
        default:err("invalid symbol type %d.", st);
    }
    return NULL;
}
Type* get_type_from_name_(Name name) {
    for (int i = 0; i < sizeof(known_types)/sizeof(known_types[0]); i++) {
        if (name_cmp(name, known_types[i].name)) {
            return &known_types[i];
        }
    }
    return NULL;
}
Name* get_name_from_type_(Type t) {
    for (int i = 0; i < sizeof(known_types)/sizeof(known_types[0]); i++) {
        if (known_types[i].type == t.type
            && known_types[i].size == t.size) {
            return &known_types[i].name;
        }
    }
    err("type not known");
    return NULL;
}

const char* optype_to_string(OpType op) {
    switch (op) {
        case OpAdd:     return "Add";
        case OpSub:     return "Sub";
        case OpMlt:     return "Mul";
        case OpDiv:     return "Div";
        case OpMod:     return "Mod";

        case OpAnd:     return "BitAnd";
        case OpOr:      return "BitOr";
        case OpXor:     return "BitXor";

        case OpLSh:     return "LShift";
        case OpRSh:     return "RShift";

        case OpOrOr:    return "LogicalOr";
        case OpAndAnd:  return "LogicalAnd";

        case OpEq:      return "Equal";
        case OpNeq:     return "NotEqual";

        case OpLt:      return "Less";
        case OpGt:      return "Greater";
        case OpLe:      return "LessEqual";
        case OpGe:      return "GreaterEqual";

        case OpAssign:  return "Assign";

        case OpNone:    return "None";

        default:
            err("unknown op type: %d", op);
            assert(0);
            return "Unknown";
    }
}

void print_indent(size_t k) {
    for (int i = 0; i < k; i++) printf(" ");
}
void print_type(const Type* type, int indent) {
    if (!type) {
		err("Invalid node type for type.");
		assert(0);
        printf("<?>");
        return;
    }
    
    switch (type->type) {
        case tt_ptr:
            printf("*");
            if (type->ptr) print_type(type->ptr, 0);
            break;
        case tt_array:
            printf("[");
            printf("?");
            /*if (type->array.size) {
                // Print array size if it's a simple number
                if (type->array.size->type == NodeNumLit) {
                    printf("%.0f", type->array.size->number.number);
                } else {
                    printf("?");
                }
            }*/ 
            printf("]");
            if (type->static_array.type) print_type(type->static_array.type, 0);
            break;
        case tt_void:
            printf("void");
            break;
        case tt_none:
        case tt_to_determinate:
			// err("Invalid node type for type (to_determinate).");
            printf("<to determinate ?>");
            break;
        default:
            if (type->name.length > 0) {
                printf("%.*s", (int)type->name.length, type->name.name);
            } else {
                printf("<%d>", type->type);
            }
            break;
    }
}

const char* unary_type_to_string(UnaryType type) {
    switch (type) {
        case UnRef:        return "&";
        case UnDeref:      return "*";
        case UnNegative:   return "-";
        case UnCompliment: return "~";
        case UnNot:        return "!";
        case UnNone:       return "?";
        default:           return "<(unknown) ?>";
    }
}

void print_node(Node* node, int indent) {
    fflush(stdout);
    
    if (!node) {
        printf("%*s<NULL>\n", indent, "");
        return;
    }
    
    print_indent(indent);
    
	// info("%s",node_type_to_string(node->type));
    switch (node->type) {
        case NodeNone:
            printf("None\n");
            break;
            
        case NodeCast:
            printf("Cast to ");
            if (node->cast.to) {
                if (node->cast.to->type == NodeTypeData) {
                    print_type(&node->cast.to->type_data, 0);
                } else {
					err("Invalid node type for type.");
					assert(0);
                    printf("<?>");
                }
            }
            printf(":\n");
            print_node(node->cast.expr, indent + 2);
            break;
            
        case NodeVarDec:
            printf("VarDec '%.*s': ", 
                   (int)node->var_dec.name.length, 
                   node->var_dec.name.name);
            print_type(node->var_dec.type, 0);
            if (node->var_dec.value) {
                printf(" =\n");
                print_node(node->var_dec.value, indent + 2);
            } else {
                printf("\n");
            }
            break;
            
        case NodeVar:
            printf("Var '%.*s'", 
                   (int)node->var.name.length, 
                   node->var.name.name);
            if (node->expr_type->type != tt_none && node->expr_type->type != tt_to_determinate) {
                printf(": ");
                print_type(node->expr_type, 0);
            }
            printf("\n");
            break;
            
        case NodeField:
            printf("Field (not fully implemented)\n");
            break;
            
        case NodeIndex:
            printf("Index: ");
			print_node(node->index.term, 0);
			print_indent(indent);
			printf("[");
			printf("?]\n");
            break;
            
        case NodeNumLit:
            printf("NumLit: ");
            if (node->number.str_repr.length > 0) {
                printf("%.*s", (int)node->number.str_repr.length, node->number.str_repr.name);
            } else {
                printf("%g", node->number.number);
            }
            if (node->expr_type->type != tt_none && node->expr_type->type != tt_to_determinate) {
                printf(" : ");
                print_type(node->expr_type, 0);
            }
            printf("\n");
            break;
            
        case NodeArg:
            printf("Arg '%.*s': ",
                   (int)node->arg.name.length,
                   node->arg.name.name);
            if (node->arg.type) {
                if (node->arg.type->type == NodeTypeData) {
                    print_type(&node->arg.type->type_data, 0);
                }
            }
            printf("\n");
            break;
            
        case NodeUnary:
            printf("Unary '%s':\n", unary_type_to_string(node->unary.type));
            print_node(node->unary.target, indent + 2);
            break;
            
        case NodeBinOp:
            printf("BinOp '%s'", optype_to_string(node->binop.type));
            if (node->expr_type->type != tt_none && node->expr_type->type != tt_to_determinate) {
                printf(" : ");
                print_type(node->expr_type, 0);
            }
            printf(":\n");
            print_node(node->binop.left, indent + 2);
            print_node(node->binop.right, indent + 2);
            break;
            
        case NodeFnDec:
            printf("FnDec '%.*s'(%zu args)(",
                   (int)node->fn_dec.name.length,
                   node->fn_dec.name.name, node->fn_dec.args_count);
			fflush(stdout);
            
            // Print args inline
            for (size_t i = 0; i < node->fn_dec.args_count; i++) {
                if (i > 0) printf(", ");
                Node* arg = node->fn_dec.args[i];
				// info("%zu:%s",arg, node_type_to_string(arg->type));
                if (arg && arg->type == NodeArg) {
                    printf("%.*s: ",
							(int)arg->arg.name.length, arg->arg.name.name);
                    if (arg->arg.type &&
							arg->arg.type->type == NodeTypeData) {
                        print_type(&arg->arg.type->type_data, 0);
                    }
                }
            }
            printf(") -> ");
			fflush(stdout);
            
            if (node->fn_dec.return_type && node->fn_dec.return_type->type == NodeTypeData) {
                print_type(&node->fn_dec.return_type->type_data, 0);
            } else {
                printf("void");
            }
            printf(":\n");
            
            print_node(node->fn_dec.body, indent + 2);
            break;
            
        case NodeIfElse:
            // Base if
            printf("If:\n");
            print_indent(indent + 2);
            printf("condition:\n");
            print_node(node->if_else_con.base_condition, indent + 4);
            print_indent(indent + 2);
            printf("then:\n");
            print_node(node->if_else_con.base_block, indent + 4);
            
            // Else-if chains
            for (size_t i = 0; i < node->if_else_con.count; i++) {
                print_indent(indent);
                printf("ElseIf:\n");
                print_indent(indent + 2);
                printf("condition:\n");
                print_node(node->if_else_con.alternate_conditions[i], indent + 4);
                print_indent(indent + 2);
                printf("then:\n");
                print_node(node->if_else_con.alternate_blocks[i], indent + 4);
            }
            
            // Else block
            if (node->if_else_con.else_block) {
                print_indent(indent);
                printf("Else:\n");
                print_node(node->if_else_con.else_block, indent + 2);
            }
            break;
            
        case NodeFnCall:
            printf("FnCall '%.*s'(%zu args):\n",
                   (int)node->fn_call.fn_name.length,
                   node->fn_call.fn_name.name,
                   node->fn_call.args_count);
            
            for (size_t i = 0; i < node->fn_call.args_count; i++) {
                print_indent(indent + 2);
                printf("[%zu]: ", i);
                print_node(node->fn_call.args[i], 0);
            }
            break;
            
        case NodeConditional:
            printf("Conditional (? :):\n");
            print_indent(indent + 2);
            printf("condition:\n");
            print_node(node->conditional.condition, indent + 4);
            print_indent(indent + 2);
            printf("if_true:\n");
            print_node(node->conditional.left_true, indent + 4);
            print_indent(indent + 2);
            printf("if_false:\n");
            print_node(node->conditional.right_false, indent + 4);
            break;
            
        case NodeBlock:
            printf("Block (%zu statements):\n", node->block.nodes_count);
            for (size_t i = 0; i < node->block.nodes_count; i++) {
                print_indent(indent + 2);
                printf("[%zu]: ", i);
                print_node(node->block.nodes[i], 0);
            }
            break;
            
        case NodeRet:
            printf("Return");
            if (node->ret) {
                printf(":\n");
                print_node(node->ret, indent + 2);
            } else {
                printf(" (void)\n");
            }
            break;
            
        case NodeTypeData:
            printf("Type: ");
            print_type(&node->type_data, 0);
            printf("\n");
            break;
            
        default:
            printf("<Unknown NodeType: %d>\n", node->type);
            break;
    }
    
    fflush(stdout);
}


void print_ast(AST* ast) {
    if (!ast) {
        printf("NULL AST\n");
        return;
    }
    
    printf("AST (%zu/%zu nodes)\n", ast->nodes_count, ast->max_nodes);
    for (size_t i = 0; i < ast->nodes_count; i++) {
        printf("[%zu] ", i);
        print_node(ast->nodes[i], 0);
    }
    info("For a total of %zu node allocations (?).",
         ast->arena->node_allocations);
}

int ast_add_node(AST* ast, Node* n) {
    ast->nodes[ast->nodes_count++] = n;
    if (ast->nodes_count >= ast->max_nodes) {
        ast->max_nodes *= 2;
        ast->nodes = (Node**)realloc(ast->nodes, ast->max_nodes);
        if (ast->nodes == NULL) {
            err( "Failed to reallocate memory for ast.");
            exit(1);
            return 1;
        }
    }
    return 0;
}

Node* arena_add_node(Arena* a, Node n) {
    a->node_allocations++;
    return (Node*)arena_add(a, sizeof(Node), &n);
}

void err_sym_exists(Name name) {
    err("symbol \"%.*s\" already exists.",
        (int)name.length,name.name);
}

// assumes an ideal type struct (to free)
char* print_type_to_buffer(char* buf, const Type* type) {
    if (!type) {
		err("Invalid node type for type.");
		assert(0);
        printf("<?>");
        return buf;
    }
    
    switch (type->type) {
        case tt_ptr:
            *(buf++) = '*';
            if (type->ptr)
				buf = print_type_to_buffer(buf, type->ptr);
            break;
        case tt_array:
            *(buf++) = '[';
            *(buf++) = '?';
            *(buf++) = ']';
            if (type->static_array.type)
				buf = print_type_to_buffer(buf, type->static_array.type);
            break;
        case tt_void:
            memcpy(buf, "void", 4);
			buf += 5;
            break;
        case tt_none:
        case tt_to_determinate:
			err("Invalid node type for type (to_determinate).");
            memcpy(buf, "<to determinate ?>", 18);
			buf += 18;
            break;
        default:
            if (type->name.length > 0) {
				memcpy(buf, type->name.name, type->name.length);
				buf += type->name.length;
            } else {
				memcpy(buf, "<?>", 3);
				buf += 3;
            }
            break;
    }
	return buf;
}
#define TAG_CMPT "[-cmpt-] "
void _cmptime_log_caller(const char *fmt, ...) {
    char buf[1024];
    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        TAG_CMPT,
        COLOR_DEBUG,
        fmt,
        args
    );
    va_end(args);

    if (len > 0)
        write_log(2, buf, (size_t)len);
}

