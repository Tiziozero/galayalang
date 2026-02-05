#ifndef PRINT_H
#define PRINT_H
// claudemaxxing so hard

#include "logger.h"
#include "parser.h"
#include <stdio.h>

// Forward declarations
static void print_type(const Type* type, int indent);
static void print_node(const Node* node, int indent);

// Helper function to print indentation
static inline void print_indent(int indent) {
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
}

// Helper function to print Name
static inline void print_name(const Name* name) {
    if (name && name->name && name->length > 0) {
        printf("%.*s", (int)name->length, name->name);
    } else {
        printf("<empty>");
    }
}

// Print TypeType enum
static inline const char* type_type_to_string(TypeType tt) {
    switch (tt) {
        case tt_to_determinate: return "to_determinate";
        case tt_u8:    return "u8";
        case tt_u16:   return "u16";
        case tt_u32:   return "u32";
        case tt_u64:   return "u64";
        case tt_u128:  return "u128";
        case tt_i8:    return "i8";
        case tt_i16:   return "i16";
        case tt_i32:   return "i32";
        case tt_i64:   return "i64";
        case tt_i128:  return "i128";
        case tt_f32:   return "f32";
        case tt_f64:   return "f64";
        case tt_ptr:   return "ptr";
        case tt_usize: return "usize";
        case tt_struct: return "struct";
        case tt_void:  return "void";
        default:       return "unknown";
    }
}

// Print NodeType enum
static inline const char* node_type_to_string(NodeKind nt) {
    switch (nt) {
        case NodeNone:        return "None";
        case NodeCast:        return "Cast";
        case NodeVarDec:      return "VarDec";
        case NodeVar:         return "Var";
        case NodeField:       return "Field";
        case NodeIndex:       return "Index";
        case NodeUnary:       return "Unary";
        case NodeNumLit:      return "NumLit";
        case NodeBinOp:       return "BinOp";
        case NodeFnDec:       return "FnDec";
        case NodeIfElse:      return "IfElse";
        case NodeFnCall:      return "FnCall";
        case NodeConditional: return "Conditional";
        case NodeBlock:       return "Block";
        case NodeRet:         return "Ret";
        case NodeTypeData:    return "TypeData";
        case NodePrintString: return "PrintString";
        case NodeStructDec  : return "StructDec";
        default:              return "Unknown";
    }
}

// Print UnaryType enum
static inline const char* unary_type_to_string(UnaryType ut) {
    switch (ut) {
        case UnNone:       return "None";
        case UnRef:        return "Ref(&)";
        case UnDeref:      return "Deref(*)";
        case UnNegative:   return "Negative(-)";
        case UnCompliment: return "Compliment(~)";
        case UnNot:        return "Not(!)";
        default:           return "Unknown";
    }
}

// Print OpType enum
static inline const char* op_type_to_string(OpType ot) {
    switch (ot) {
        case OpNone:   return "None";
        case OpAssign: return "=";
        case OpOrOr:   return "||";
        case OpAndAnd: return "&&";
        case OpOr:     return "|";
        case OpXor:    return "^";
        case OpAnd:    return "&";
        case OpEq:     return "==";
        case OpNeq:    return "!=";
        case OpLt:     return "<";
        case OpGt:     return ">";
        case OpLe:     return "<=";
        case OpGe:     return ">=";
        case OpLSh:    return "<<";
        case OpRSh:    return ">>";
        case OpAdd:    return "+";
        case OpSub:    return "-";
        case OpMlt:    return "*";
        case OpDiv:    return "/";
        case OpMod:    return "%";
        case OpComma:  return ",";
        default:       return "Unknown";
    }
}

// Print Type
static void print_type(const Type* type, int indent) {
    if (!type) {
        printf("<null type>");
        return;
    } else;
        // printf("<%zu>", (size_t)type);
    
    printf("Type { %s, size=%zu, name=", 
           type_type_to_string(type->type), type->size);
    print_name(&type->name);
    
    if (type->type == tt_ptr) {
        printf(", points_to=");
        print_type(type->ptr, 0);
    }
    
    printf(" }");
}

// Print Variable
static void print_variable(const Variable* var, int indent) {
    print_indent(indent);
    printf("Variable { name=");
    print_name(&var->name);
    printf(", type=");
    print_type(var->type, 0);
    printf(", mutable=%s }\n", var->is_mutable ? "yes" : "no");
}

// Print Argument
static void print_argument(const Argument* arg, int indent) {
    print_indent(indent);
    printf("Argument { name=");
    print_name(&arg->name);
    printf(", type=");
    print_type(arg->type, 0);
    printf(", mutable=%s }\n", arg->is_mutable ? "yes" : "no");
}

// Print Function
static void print_function(const Function* fn, int indent) {
    print_indent(indent);
    printf("Function { name=");
    fflush(stdout);
    print_name(&fn->name);
    printf(", args_count=%zu\n", fn->args_count);
    
    for (size_t i = 0; i < fn->args_count; i++) {
        print_argument(&fn->args[i], indent + 1);
    }
    
    print_indent(indent + 1);
    printf("return_type=");
    print_type(fn->return_type, 0);
    printf("\n");
    
    if (fn->body) {
        print_indent(indent + 1);
        printf("body:\n");
        print_node(fn->body, indent + 2);
    }
    print_indent(indent);
    printf("}\n");
}

// Print Node
static void print_node(const Node* node, int indent) {
    if (!node) {
        print_indent(indent);
        printf("<null node>\n");
        return;
    }
    
    print_indent(indent + 1);
    fflush(stdout);
    printf("resulting_type=");
    fflush(stdout);
    if (node->type.state == TsOk) {
        print_type(node->type.type, 0);
    } else {
        if (is_untyped((Node*)node)) {
            printf("[is untyped]");
        } else {
            if (node->kind == NodeIfElse) {
                printf("<no type>");
            } else {
                err("Failed tp check type in node %s (state is %zu)",
                        node_type_to_string(node->kind), node->type.state);
                if (node->kind == NodeVar) {
                    print_name(&node->var.name);
                    printf("\n");
                }
                assert(0&&"Failed tp check type");
            }
        }
    }
    fflush(stdout);
    printf(", state=%d\n", node->type.state);
    fflush(stdout);
    
    switch (node->kind) {
        case NodeVar:
            print_variable(&node->var, indent + 1);
            break;
            
        case NodeVarDec:
            print_indent(indent + 1);
            printf("VarDec { name=");
            print_name(&node->var_dec.name);
            printf(", type=");
            print_type(node->var_dec.type, 0);
            printf("\n");
            if (node->var_dec.value) {
                print_indent(indent + 1);
                printf("value:\n");
                print_node(node->var_dec.value, indent + 2);
            }
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeNumLit:
            print_indent(indent + 1);
            printf("Number { value=%g, type=", node->number.number);
            print_type(node->type.type, 0);
            printf(", str=");
            print_name(&node->number.str_repr);
            printf(" }\n");
            break;
            
        case NodeUnary:
            print_indent(indent + 1);
            printf("Unary { op=%s\n", unary_type_to_string(node->unary.type));
            print_node(node->unary.target, indent + 2);
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeBinOp:
            print_indent(indent + 1);
            printf("BinOp { op=%s\n", op_type_to_string(node->binop.type));
            print_indent(indent + 1);
            printf("left:\n");
            print_node(node->binop.left, indent + 2);
            print_indent(indent + 1);
            printf("right:\n");
            print_node(node->binop.right, indent + 2);
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeFnDec:
            print_function(&node->fn_dec, indent + 1);
            break;
            
        case NodeFnCall:
            print_indent(indent + 1);
            printf("FnCall { name=");
            print_name(&node->fn_call.fn_name);
            printf(", args_count=%zu\n", node->fn_call.args_count);
            for (size_t i = 0; i < node->fn_call.args_count; i++) {
                print_node(node->fn_call.args[i], indent + 2);
            }
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeBlock:
            print_indent(indent + 1);
            printf("Block { nodes_count=%zu\n", node->block.nodes_count);
            for (size_t i = 0; i < node->block.nodes_count; i++) {
                print_node(node->block.nodes[i], indent + 2);
            }
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeIfElse:
            print_indent(indent + 1);
            printf("IfElse {\n");
            print_indent(indent + 2);
            printf("base_condition:\n");
            print_node(node->if_else_con.base_condition, indent + 3);
            print_indent(indent + 2);
            printf("base_block:\n");
            print_node(node->if_else_con.base_block, indent + 3);
            
            for (size_t i = 0; i < node->if_else_con.count; i++) {
                print_indent(indent + 2);
                printf("alternate[%zu] condition:\n", i);
                print_node(node->if_else_con.alternate_conditions[i], indent + 3);
                print_indent(indent + 2);
                printf("alternate[%zu] block:\n", i);
                print_node(node->if_else_con.alternate_blocks[i], indent + 3);
            }
            
            if (node->if_else_con.else_block) {
                print_indent(indent + 2);
                printf("else_block:\n");
                print_node(node->if_else_con.else_block, indent + 3);
            }
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeConditional:
            print_indent(indent + 1);
            printf("Conditional {\n");
            print_indent(indent + 2);
            printf("condition:\n");
            print_node(node->conditional.condition, indent + 3);
            print_indent(indent + 2);
            printf("if_true:\n");
            print_node(node->conditional.left_true, indent + 3);
            print_indent(indent + 2);
            printf("if_false:\n");
            print_node(node->conditional.right_false, indent + 3);
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeRet:
            print_indent(indent + 1);
            printf("Return {\n");
            print_node(node->ret, indent + 2);
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeCast:
            print_indent(indent + 1);
            printf("Cast { to=");
            print_type(node->cast.to, 0);
            printf("\n");
            print_node(node->cast.expr, indent + 2);
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodeIndex:
            print_indent(indent + 1);
            printf("Index {\n");
            print_indent(indent + 2);
            printf("base:\n");
            print_node(node->index.target, indent + 3);
            print_indent(indent + 2);
            printf("index:\n");
            print_node(node->index.index_expression, indent + 3);
            print_indent(indent + 1);
            printf("}\n");
            break;
            
        case NodePrintString:
            print_indent(indent + 1);
            printf("PrintString { ");
            print_name(&node->print_string.string);
            printf(" }\n");
            break;
            
        default:
            print_indent(indent + 1);
            printf("<unhandled node type>\n");
            break;
    }
    
    print_indent(indent);
    printf("}\n");
}

// Print Symbol
static void print_symbol(const Symbol* sym, int indent) {
    if (!sym) {
        print_indent(indent);
        printf("<null symbol>\n");
        return;
    }
    
    print_indent(indent);
    switch (sym->sym_type) {
        case SymVar:
            printf("Symbol(Var) {\n");
            print_variable(&sym->var, indent + 1);
            break;
        case SymFn:
            printf("Symbol(Fn) {\n");
            print_function(&sym->fn, indent + 1);
            break;
        case SymArg:
            printf("Symbol(Arg) {\n");
            print_argument(&sym->argument, indent + 1);
            break;
        case SymType:
            printf("Symbol(Type) {\n");
            print_indent(indent + 1);
            print_type(&sym->type, indent + 1);
            printf("\n");
            break;
        case SymField:
            printf("Symbol(Field) {\n");
            print_indent(indent + 1);
            printf("Field { name=");
            print_name(&sym->field.name);
            printf(", type=");
            print_type(sym->field.type, 0);
            printf(", mutable=%s }\n", sym->field.is_mutable ? "yes" : "no");
            break;
        default:
            printf("Symbol(Unknown)\n");
            break;
    }
    print_indent(indent);
    printf("}\n");
}

// Print SymbolStore
static void print_symbol_store(const SymbolStore* ss, int indent) {
    if (!ss) {
        print_indent(indent);
        printf("<null symbol store>\n");
        return;
    }
    
    print_indent(indent);
    printf("SymbolStore { symbols_count=%zu\n", ss->syms_count);
    
    for (size_t i = 0; i < ss->syms_count; i++) {
        print_symbol(&ss->syms[i], indent + 1);
    }
    
    if (ss->parent) {
        print_indent(indent + 1);
        printf("parent:\n");
        print_symbol_store(ss->parent, indent + 2);
    }
    
    print_indent(indent);
    printf("}\n");
}

// Print AST
static void print_ast(const AST* ast) {
    if (!ast) {
        printf("<null AST>\n");
        return;
    }
    
    printf("AST { nodes_count=%zu, max_nodes=%zu\n", ast->nodes_count, ast->max_nodes);
    
    for (size_t i = 0; i < ast->nodes_count; i++) {
        printf("\n--- Node %zu ---\n", i);
        print_node(ast->nodes[i], 0);
    }
    
    printf("}\n");
}

// Print ParserCtx
static void print_parser_ctx(const ParserCtx* ctx) {
    if (!ctx) {
        printf("<null parser context>\n");
        return;
    }
    
    printf("ParserCtx {\n");
    printf("  tokens_count=%zu, tokens_index=%zu\n", ctx->tokens_count, ctx->tokens_index);
    
    printf("\n  === Symbols ===\n");
    print_symbol_store(&ctx->symbols, 1);
    
    printf("\n  === AST ===\n");
    print_ast(ctx->ast);
    
    printf("}\n");
}

#endif // PRINT_H
