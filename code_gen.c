#include "code_gen.h"
#include "parser.h"
#include "print.h"
#include "utils.h"
#include <stdio.h>
/*#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>*/
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>

#define RV_FAIL 0
#define RV_NUM 1
#define RV_PTR 2

char* buf_write_name(char** buf, Name name) {
    if (!name.name || !name.length) return 0;
    memcpy(*buf, name.name, name.length);
    *buf += name.length;
    return *buf;
}
char* buf_write_cstr(char** buf, char* s) {
    size_t len = strlen(s);
    memcpy(*buf, s, len);
    *buf += len;
    return *buf;
}
char* buf_write_char(char** buf, char c) {
    **buf = c;
    (*buf)++;
    return *buf;
}
char* buf_write_c_type(char** buf, Type t) {
    if (t.kind == tt_ptr) {
        buf_write_c_type(buf, *t.ptr);
        buf_write_char(buf, '*');
        return *buf;
    } else if (t.kind == tt_ptr) { // do the same as ptr
                                   // and pray for a not out of bound
        buf_write_c_type(buf, *t.ptr);
        buf_write_char(buf, '*');
        return *buf;
    }
    buf_write_name(buf, t.name);
    return *buf;
}

char* gen_c(ParserCtx* pctx, char** buf, Node* node);

char* expression_to_buf(char** buf, Node* node) {
    // buf_write_char(buf, '(');
    switch (node->kind) {
        case NodeCast: 
            buf_write_cstr(buf, "(");
            buf_write_c_type(buf, *node->cast.to);
            buf_write_cstr(buf, ")");
            expression_to_buf(buf, node->cast.expr);
            break;
        case NodeBinOp:
            buf_write_char(buf, '(');
            expression_to_buf(buf, node->binop.left);
            switch (node->binop.type) {
                case OpAssign:  buf_write_char(buf,'='); break;
                case OpSub:     buf_write_char(buf,'-'); break;
                case OpAdd:     buf_write_char(buf,'+'); break;
                case OpMlt:     buf_write_char(buf,'*'); break;
                case OpDiv:     buf_write_char(buf,'/'); break;
                case OpLe:      buf_write_cstr(buf, "<="); break;
                case OpGe:      buf_write_cstr(buf, ">="); break;
                case OpLt:      buf_write_cstr(buf, "<"); break;
                case OpGt:      buf_write_cstr(buf, ">"); break;
                case OpEq:      buf_write_cstr(buf, "=="); break;
                case OpNeq:     buf_write_cstr(buf, "!="); break;
                default: panic("binop Unimplemented %zu", node->binop.type);
            }
            expression_to_buf(buf, node->binop.right);
            buf_write_char(buf, ')');
            break;
        case NodeUnary:
            buf_write_char(buf, '(');
            switch (node->unary.type) {
                case UnRef:         buf_write_char(buf, '&'); break;
                case UnDeref:       buf_write_char(buf, '*'); break;
                case UnNegative:    buf_write_char(buf, '-'); break;
                case UnNot:         buf_write_char(buf, '!'); break;
                case UnCompliment:  buf_write_char(buf, '~'); break;
                default: panic("unary Unimplemented %zu", node->unary.type);
            }
            expression_to_buf(buf, node->unary.target);
            buf_write_char(buf, ')');
            break;
        case NodeNumLit:
            buf_write_name(buf, node->number.str_repr);
            break;
        case NodeVar:
            buf_write_name(buf, node->var.name);
            break;
        case NodeFnCall:
            buf_write_name(buf, node->fn_call.fn_name);
            buf_write_char(buf, '(');
            for (size_t i = 0; i < node->fn_call.args_count; i++) {
                if (i > 0) buf_write_char(buf, ','); // comma for next arg
                expression_to_buf(buf, node->fn_call.args[i]);
            }
            buf_write_char(buf, ')');
            break;
        case NodeFieldAccess:
            expression_to_buf(buf, node->field_access.target);
            buf_write_cstr(buf, ".");
            buf_write_name(buf, node->field_access.name);
            break;
        case NodeUntypedStruct:
            buf_write_cstr(buf, "{");
            for (size_t i = 0; i < node->untyped_strcut.count; i++) {
                if (i > 0) {
                    buf_write_cstr(buf, ", ");
                }
                buf_write_cstr(buf, ".");
                buf_write_name(buf, node->untyped_strcut.fields[i].name);
                buf_write_cstr(buf, "=");
                expression_to_buf(buf,node->untyped_strcut.fields[i].expr);
            }
            buf_write_cstr(buf, "}");
            break;
        case NodeStringLit:
            buf_write_char(buf, '\"');
            buf_write_name(buf, node->string_literal);
            buf_write_char(buf, '\"');
            break;
        default:
            panic("Invalid (expression) Node type %zu %s", node->kind, node_type_to_string(node->kind));
            // assert(0);
            return 0;
    }
    // buf_write_char(buf, ')');
    return *buf;
}
char* gen_c(ParserCtx* pctx, char** buf, Node* node) {
    // info("%s", node_type_to_string(node->type));
    switch (node->kind) {
        case NodeVarDec:
           buf_write_c_type(buf, *node->var_dec.type);
           buf_write_char(buf, ' ');
           buf_write_name(buf, node->var_dec.name);
            if (!node->var_dec.value) {
                buf_write_char(buf, ';');
            } else {
                buf_write_char(buf, ' ');
                buf_write_char(buf, '=');
                buf_write_char(buf, ' ');
                expression_to_buf(buf, node->var_dec.value);
                buf_write_char(buf, ';');
            }
            break;
        case NodeFnDec:
            buf_write_c_type(buf, *node->fn_dec.return_type);
            buf_write_char(buf, ' ');
            buf_write_name(buf, node->fn_dec.name);
            buf_write_char(buf, '(');
            for (size_t i = 0; i < node->fn_dec.args_count; i++) {
                Argument arg = node->fn_dec.args[i];
                if (i > 0) buf_write_char(buf, ',');
                buf_write_c_type(buf, *arg.type);
                buf_write_char(buf, ' ');
                buf_write_name(buf, arg.name);
            }
            buf_write_char(buf, ')');
            if (node->fn_dec.body) {
                buf_write_char(buf, '{');
                buf_write_char(buf, '\n');
                gen_c(pctx, buf, node->fn_dec.body);
                buf_write_char(buf, '}');
            } else {
                buf_write_char(buf, ';');
            }
            break;
        case NodeBlock: // will not inlude "{}";
            for (size_t i = 0; i < node->block.nodes_count; i++) {
                if (!gen_c(pctx, buf, node->block.nodes[i])) return 0;
            }
            break;
        case NodeBinOp:
            expression_to_buf(buf, node);
            buf_write_char(buf, ';');
            break;
        case NodeIfElse:
            buf_write_cstr(buf, "if (");
            expression_to_buf(buf, node->if_else_con.base_condition);
            buf_write_cstr(buf, ") {\n");
            if (!gen_c(pctx, buf, node->if_else_con.base_block)) return 0;
            buf_write_cstr(buf, "}\n");
            break;
        case NodeRet:
            buf_write_cstr(buf, "return ");
            expression_to_buf(buf, node->ret);
            buf_write_char(buf, ';');
            break;
        case NodeCast:
            buf_write_cstr(buf, "(");
            buf_write_c_type(buf, *node->cast.to);
            buf_write_cstr(buf, ")");
            expression_to_buf(buf, node->cast.expr);
            break;
        case NodeStructDec:
            buf_write_cstr(buf, "typedef struct {\n");
            for (size_t i = 0; i < node->struct_dec.fields_count; i++) {
                buf_write_c_type(buf, *node->struct_dec.fields[i].type);
                buf_write_cstr(buf, " ");
                buf_write_name(buf, node->struct_dec.fields[i].name);
                buf_write_cstr(buf, ";\n");
            }
            buf_write_cstr(buf, "}");
            buf_write_name(buf, node->struct_dec.name);
            buf_write_cstr(buf, ";\n");
            break;
        case NodeFnCall:
            expression_to_buf(buf, node);
            buf_write_cstr(buf, ";\n");
            break;
        case NodePrintString: break; // skip
        default:
            panic("Invalid Node type %zu %s", node->kind, node_type_to_string(node->kind));
            // assert(0);
            return 0;
    }
    buf_write_char(buf, '\n');
    return *buf;
}
int code_gen(ParserCtx* pctx) {
    AST* ast = pctx->ast;
    char buf[1024*1024];
    char* path = "gala.out.c";
    FILE* f = fopen(path, "wb");
    if (!f) {
        err("Failed to open output file.");
        return 0;
    }
    // codegen_init(path);
    fprintf(f, "// generated using uqc, the galayalang compiler\n"
            "void print();\n"
            "#include <stdint.h>\n"
            // "#include <stdlib.h>\n"
            // "#include <string.h>\n"
            "typedef uint8_t    u8;\n"
            "typedef uint16_t   u16;\n"
            "typedef uint32_t   u32;\n"
            "typedef uint64_t   u64;\n"
            // "typedef uint128_t  u128;\n"
            "typedef int8_t     i8;\n"
            "typedef int16_t    i16;\n"
            "typedef int32_t    i32;\n"
            "typedef int64_t    i64;\n"
            "typedef float      f32;\n"
            "typedef double     f64;\n"
            "typedef u64        usize;\n"
            "// GALASTART\n"
            // "typedef int128_t   i128;\n"
            // "#include <stdio.h>\n"
            );
    for (size_t i = 0; i < ast->nodes_count; i++) {
        Node* node = ast->nodes[i];
        char original_buf[1024*32];
        char* buf = original_buf;
        memset(buf, 0, 1024*32);
        if (!gen_c(pctx, &buf, node)) {
            err("Failed to gen code.");
            fclose(f);
            return 0;
        }
        fprintf(f, "%s", original_buf);
    }
    fprintf(f, "// GALAEND\n");
    fprintf(f, "#include <unistd.h> // for syscall numbers (optional, can use numbers directly)\n");
    fprintf(f, "void print_string(const char *s) {\n");
    fprintf(f, "// Linux x86_64 syscall: write(fd=1, buf=s, count=len)\n");
    fprintf(f, "const char *p = s;\n");
    fprintf(f, "long len = 0;\n");
    fprintf(f, "\n");
    fprintf(f, "// Compute string length manually (no strlen)\n");
    fprintf(f, "while (p[len] != '\\0') {\n");
    fprintf(f, "len++;\n");
    fprintf(f, "}\n");
    fprintf(f, "\n");
    fprintf(f, "// syscall: write(1, s, len)\n");
    fprintf(f, "asm volatile(\n");
    fprintf(f, "\"movq $1,  %%%%rax  \\n\"  // syscall number 1 = sys_write\n");
    fprintf(f, "\"movq $1,  %%%%rdi  \\n\"  // fd = 1 (stdout)\n");
    fprintf(f, "\"movq %%0, %%%%rsi \\n\"  // buffer\n");
    fprintf(f, "\"movq %%1, %%%%rdx \\n\"  // length\n");
    fprintf(f, "\"syscall\"\n");
    fprintf(f, ":\n");
    fprintf(f, ": \"r\"(s), \"r\"(len)\n");
    fprintf(f, ": \"rax\", \"rdi\", \"rsi\", \"rdx\"\n");
    fprintf(f, ");\n");
    fprintf(f, "}\n");
    fprintf(f, "\n");
    fprintf(f, "void _start() {\n");
    fprintf(f, "print_string(\"Hello from Linux syscall!\\n\");\n");
    fprintf(f, "\n");
    fprintf(f, "main(); // call main\n");
    fprintf(f, "// exit(0) without libc\n");
    fprintf(f, "asm volatile(\n");
    fprintf(f, "\"movq $60, %%%%rax \\n\" // syscall number 60 = exit\n");
    fprintf(f, "\"xor %%%%rdi, %%%%rdi \\n\" // exit code 0\n");
    fprintf(f, "\"syscall\"\n");
    fprintf(f, ":\n");
    fprintf(f, ":\n");
    fprintf(f, ": \"rax\", \"rdi\"\n");
    fprintf(f, ");\n");
    fprintf(f, "}\n");
    fprintf(f, "void print() { print_string(\"Print Function called.!!!\\n\"); }");
    fclose(f);
    // system("echo \"Output file:\"");
    // system("cat gala.out.c");
    // int ret = system("clang -o output gala.out.c");
    int ret = system("gcc -c -nostdlib -o output.o gala.out.c");
    
    if (ret != 0) {
        panic("Failed to compile c file.");
        return 0;
    }
    return 1;
}

