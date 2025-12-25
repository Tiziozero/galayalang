#include "code_gen.h"
#include "parser.h"
#include <iso646.h>
#include <stdio.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>


typedef struct {
    LLVMModuleRef module;
    LLVMBuilderRef builder;
    LLVMContextRef context;
    // Symbol table for variables/functions
    // HashMap or similar for Name -> LLVMValueRef
} Codegen;

Codegen* codegen_init(const char* module_name) {
    Codegen* cg = malloc(sizeof(Codegen));
    cg->context = LLVMContextCreate();
    cg->module = LLVMModuleCreateWithNameInContext(module_name, cg->context);
    cg->builder = LLVMCreateBuilderInContext(cg->context);
    return cg;
}

LLVMValueRef codegen_node(Codegen* cg, Node* node) {
    switch (node->type) {
        case NodeNumLit:
            return LLVMConstReal(LLVMDoubleType(), node->number.number);
            
        case NodeBinOp: {
            LLVMValueRef left = codegen_node(cg, node->binop.left);
            LLVMValueRef right = codegen_node(cg, node->binop.right);
            
            switch (node->binop.type) {
                case OpAdd:
                    return LLVMBuildFAdd(cg->builder, left, right, "addtmp");
                case OpSub:
                    return LLVMBuildFSub(cg->builder, left, right, "subtmp");
                case OpMlt:
                    return LLVMBuildFMul(cg->builder, left, right, "multmp");
                case OpDiv:
                    return LLVMBuildFDiv(cg->builder, left, right, "divtmp");
                // ... other ops
                default: {
                    err("Unknown BinOp: %zu", node->binop.type);
                    assert(0);
                }
            }
            break;
        }
        
        case NodeFnDec: {
            // Create function type
            LLVMTypeRef ret_type = LLVMDoubleType(); // or your return type
            LLVMTypeRef param_types[] = { /* your params */ };
            LLVMTypeRef fn_type = LLVMFunctionType(ret_type, param_types, 0, 0);
            
            // Create function
            // LLVMValueRef fn = LLVMAddFunction(cg->module, node->fn_dec.name, fn_type);
            
            // Create entry block
            // LLVMBasicBlockRef entry = LLVMAppendBasicBlock(fn, "entry");
            // LLVMPositionBuilderAtEnd(cg->builder, entry);
            
            // Generate body
            LLVMValueRef body = codegen_node(cg, node->fn_dec.body);
            LLVMBuildRet(cg->builder, body);
            
            // return fn;
        }
        
        case NodeFnCall: {
            // LLVMValueRef fn = LLVMGetNamedFunction(cg->module, node->fn_call.name);
            // Get args and call
            // return LLVMBuildCall2(cg->builder, /* type */, fn, /* args */, 0, "calltmp");
        }
        default: {
            err("Unknown Node Type: %zu", node->type);
            assert(0);
        }
        
        // ... other node types
    }
    return NULL;
}
// Option 1: Output LLVM IR (human-readable)
void output_ir(Codegen* cg, const char* filename) {
    char* ir = LLVMPrintModuleToString(cg->module);
    FILE* f = fopen(filename, "w");
    fprintf(f, "%s", ir);
    fclose(f);
    LLVMDisposeMessage(ir);
}

// Option 2: Output bitcode
void output_bitcode(Codegen* cg, const char* filename) {
    LLVMWriteBitcodeToFile(cg->module, filename);
}

// Option 3: JIT compilation
void jit_run(Codegen* cg) {
    LLVMLinkInMCJIT();
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    
    LLVMExecutionEngineRef engine;
    char* error = NULL;
    
    if (LLVMCreateExecutionEngineForModule(&engine, cg->module, &error)) {
        fprintf(stderr, "Failed to create execution engine: %s\n", error);
        LLVMDisposeMessage(error);
        return;
    }
    
    // Get function and run it
    LLVMValueRef fn = LLVMGetNamedFunction(cg->module, "main");
    LLVMGenericValueRef result = LLVMRunFunction(engine, fn, 0, NULL);
}
/*
# After generating LLVM IR (.ll) or bitcode (.bc):
clang output.ll -o program
# or
llc output.ll -o output.s  # Generate assembly
clang output.s -o program  # Assemble and link
*/

#define RV_FAIL 0
#define RV_NUM 1
#define RV_PTR 2

// skip_paren_around_expr is for assignment or dereference or other nodes that C doesn't want wrapped in parenthesis
int get_expression_as_name_node(Node* n,Name* name,
                                int skip_paren_around_expr) {
    int rv = RV_NUM;
    // char* original = name->name;
    if (n->type == NodeNumLit) {
        memcpy(name->name, n->number.str_repr.name,n->number.str_repr.length);
        name->name += n->number.str_repr.length;
        name->length += n->number.str_repr.length;
        return 1;
    } else if (n->type == NodeBinOp) {
        if (!skip_paren_around_expr) {
            *name->name = '(';
            name->name++;
            name->length++;
        }
        // can not assign an expression eg: (ident) = expr;
        // that is invalid
        if (!get_expression_as_name_node(
            n->binop.left, name, n->binop.type == OpAssign ? 1 : 0)) return 0;
        switch (n->binop.type) {
            case OpAdd: *name->name = '+'; break;
            case OpSub: *name->name = '-'; break;
            case OpMlt: *name->name = '*'; break;
            case OpDiv: *name->name = '/'; break;
            case OpOr: *name->name = '|'; break;
            case OpAnd: *name->name = '&'; break;
            case OpAssign: *name->name = '='; break;
            default: TODO("Unimplemented");
        }
        name->name++;
        name->length++;
        if (!get_expression_as_name_node(
            n->binop.right, name, n->binop.type == OpAssign ? 1 : 0)) return 0;
        if (!skip_paren_around_expr) {
            *name->name = ')';
            name->name++;
            name->length++;
        }
        return RV_NUM;
    } else if (n->type == NodeVar) {
        memcpy(name->name, n->var.name.name, n->var.name.length);
        name->name += n->var.name.length;
        name->length += n->var.name.length;
        return RV_NUM;
    } else if (n->type == NodeUnary) {
        switch (n->unary.type) {
            case UnDeref: *name->name = '*';break;
            case UnRef: *name->name = '&';break;
            case UnNot: *name->name = '!';break;
            case UnNegative: *name->name = '-';break;
            case UnCompliment: *name->name = '~';break;
            default: TODO("unhandeled");
        }
        name->name++;
        name->length++;

        // *name->name = '(';
        // name->name++;
        // name->length++;
        if ((rv=get_expression_as_name_node(n->unary.target, name, 1)) == 0) return 0;
        if (n->unary.type == UnRef) rv = RV_PTR;
        return rv;
        // *name->name = ')';
        // name->name++;
        // name->length++;
    } else {
        err("unhandeled node: %s.", get_node_data(n));
        TODO("finish function");
    }
    return RV_NUM;
}

struct Expr {
    Name name;
    int type;
};
struct Expr get_expression_as_name(Node* node) {
    Name name;
    name.name = malloc(1024); // make it a large expression
    char* original = name.name; // keep original ptr
    memset(name.name, 0, 1024);
    int rv = get_expression_as_name_node(node, &name, 0); // get expr
    name.name = original;
    return (struct Expr){.name=name, .type=rv};
}

int gen_c(FILE* f, Node node) {
    switch (node.type) {
        case NodeVarDec: {
            if (node.var_dec.value == NULL) {
                fprintf(f, "int %.*s;\n",
                        (int)node.var_dec.name.length,
                        node.var_dec.name.name);
            } else {
                struct Expr expr  = get_expression_as_name(node.var_dec.value);
                if (expr.type == RV_PTR) {
                    fprintf(f, "int* %.*s = %.*s;\n",
                            (int)node.var_dec.name.length,
                            node.var_dec.name.name,
                            (int)expr.name.length, expr.name.name);
                } else {
                    fprintf(f, "int %.*s = %.*s;\n",
                            (int)node.var_dec.name.length,
                            node.var_dec.name.name,
                            (int)expr.name.length, expr.name.name);
                }
                free(expr.name.name);
            }
        } break;
        case NodeFnDec: {
            fprintf(f, "int %.*s () {\n", (int)node.fn_dec.name.length,
                    node.fn_dec.name.name);
            Node** nodes = node.fn_dec.body->block.nodes;
            for (size_t i = 0;
            i < node.fn_dec.body->block.nodes_count; i++) {
                Node* cur_node = nodes[i];
                if (!gen_c(f, *cur_node)) return 0;
            }

            fprintf(f, "}\n");
        } break;
        case NodeUnary:case NodeBinOp: {
            struct Expr expr = get_expression_as_name(&node);
            if (expr.type == RV_PTR) {
                fprintf(f, "%.*s;\n", (int)expr.name.length, expr.name.name);
            } else 
                fprintf(f, "%.*s;\n", (int)expr.name.length, expr.name.name);
        } break;
        case NodeRet: {
            struct Expr expr = get_expression_as_name(node.ret);
            fprintf(f, "return %.*s;\n",
                    (int)expr.name.length, expr.name.name);
        } break;
        default: err("Invalid node: %s", node_type_to_string(node.type)); return 0;
    }
    return 1;
}
// returns 1 on success
int code_gen(AST* ast) {
    return 1;
    char* path = "gala.out.c";
    FILE* f = fopen(path, "wb");
    if (!f) {
        err("Failed to open output file.");
        return 0;
    }
    // codegen_init(path);
    fprintf(f, "// generated using uqc, the galayalang compiler\n"
            "#include <stdint.h>\n"
            "#include <stdio.h>\n");
    for (size_t i = 0; i < ast->nodes_count; i++) {
        Node node = *ast->nodes[i];
        if (!gen_c(f, node)) {
            err("Failed to gen code.");
            fclose(f);
            return 0;
        }
    }
    fclose(f);
    system("echo \"Output file:\"");
    system("cat gala.out.c");
    system("clang -o prog gala.out.c");
    int ret = system("./prog");

    if (ret == -1) {
        perror("system");
    } else {
        int exit_code = WEXITSTATUS(ret);
        printf("Exit code: %d\n", exit_code);
    }

    system("rm ./gala.out.c");
    return 1;
}

