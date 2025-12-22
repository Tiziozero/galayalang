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

typedef struct {
    LLVMModuleRef module;
    LLVMBuilderRef builder;
    LLVMContextRef context;
    // Symbol table for variables/functions
    // HashMap or similar for Name -> LLVMValueRef
} Codegen;

Codegen* codegen_init(const char* module_name) {
    info("Code gen init...");
    Codegen* cg = malloc(sizeof(Codegen));
    cg->context = LLVMContextCreate();
    cg->module = LLVMModuleCreateWithNameInContext(module_name, cg->context);
    cg->builder = LLVMCreateBuilderInContext(cg->context);
    return cg;
}

LLVMValueRef codegen_node(Codegen* cg, Node* node) {
    info("Codegen node");
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


int print_expression(Node* n,Name* name) {
    char* original = name->name;
    if (n->type == NodeNumLit) {
        memcpy(name->name, n->number.str_repr.name,n->number.str_repr.length);
        name->name += n->number.str_repr.length;
        name->length += n->number.str_repr.length;
        printf("number: %.*s\n", (int)name->length, name->name);
    } else if (n->type == NodeBinOp) {
        *name->name = '(';
        name->name++;
        name->length++;
        if (!print_expression(n->binop.left, name)) return 0;
        printf("Op: %s\n", optype_to_string(n->binop.type));
        switch (n->binop.type) {
            case OpAdd: *name->name = '+'; break;
            case OpSub: *name->name = '-'; break;
            case OpMlt: *name->name = '*'; break;
            case OpDiv: *name->name = '/'; break;
            case OpOr: *name->name = '|'; break;
            case OpAnd: *name->name = '&'; break;
            default: TODO("Unimplemented");
        }
        name->name++;
        name->length++;
        if (!print_expression(n->binop.right, name)) return 0;
        *name->name = ')';
        name->name++;
        name->length++;
    }
    return 1;
}


int code_gen(AST* ast) {
    char* path = "gala.out.c";
    info("path: %s.", path);
    FILE* f = fopen(path, "wb");
    if (!f) {
        err("Failed to open output file.");
        return 0;
    }
    // codegen_init(path);
    fprintf(f, "// generated using uqc, the galayalang compiler\n#include <stdint.h>\n#include <stdio.h>\n");
    for (size_t i = 0; i < ast->nodes_count; i++) {
        Node node = *ast->nodes[i];
        fprintf(stdout, "Node: %s\n", get_node_data(&node));
        switch (node.type) {
            case NodeVarDec: {
                if (node.var_dec.value == NULL) {
                    fprintf(f, "size_t %.*s;\n",
                            (int)node.var_dec.name.length,
                            node.var_dec.name.name);
                    fprintf(stdout, "size_t %.*s;\n",
                            (int)node.var_dec.name.length,
                            node.var_dec.name.name);
                } else {
                    printf("aassignment: "); print_node(&node, 0);
                    Name name;
                    name.name = malloc(1024); // make it a large expression
                    char* original = name.name; // make it a large expression
                    memset(name.name, 0, 1024);
                    print_expression(node.var_dec.value, &name);
                    name.name = original;
                    printf("\t\tresulting expression (%d): %.*s\n",
                           (int)name.length, (int)name.length, name.name);
                    fprintf(f, "size_t %.*s = %.*s;\n",
                            (int)node.var_dec.name.length,
                            node.var_dec.name.name,
                            (int)name.length, name.name);
                    fprintf(stdout, "size_t %.*s = %.*s;\n",
                            (int)node.var_dec.name.length,
                            node.var_dec.name.name,
                            (int)name.length, name.name);
                    free(name.name);
                }
            } break;
            case NodeFnDec: {
                fprintf(f, "size_t %.*s () {\n", (int)node.fn_dec.name.length,
                        node.fn_dec.name.name);
                fprintf(stdout, "size_t %.*s () {\n", (int)node.fn_dec.name.length,
                        node.fn_dec.name.name);

                fprintf(f, "}\n");
                fprintf(stdout, "}\n");
            } break;
            default: err("Invalid node: %s", node_type_to_string(node.type)); assert(0);
        }
    }

    // fprintf(f, "Hello, World");
    fclose(f);
    system("clang -o prog gala.out.c");
    system("./prog");
    system("rm ./gala.out.c");
    return 1;
}

