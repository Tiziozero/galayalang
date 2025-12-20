#include "code_gen.h"
#include <stdio.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

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
            return LLVMConstReal(LLVMDoubleType(), node->number);
            
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
int code_gen(AST* ast) {
    char* path = "gala.out";
    FILE* f = fopen(path, "wb");
    if (!f) {
        err("Failed to open output file.");
        return 0;
    }

    fprintf(f, "Hello, World");

    fclose(f);
    return 1;
}

