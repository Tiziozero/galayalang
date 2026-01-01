#include "code_gen.h"
#include "parser.h"
#include "utils.h"
#include <iso646.h>
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
    if (t.type == tt_ptr) {
        buf_write_c_type(buf, *t.ptr);
        buf_write_char(buf, '*');
        return *buf;
    } else if (t.type == tt_ptr) { // do the same as ptr
                                   // and pray for a not out of bound
        buf_write_c_type(buf, *t.ptr);
        buf_write_char(buf, '*');
        return *buf;
    }
    buf_write_name(buf, t.name);
    return *buf;
}


char* expression_to_buf(char** buf, Node* node) {
    buf_write_char(buf, '(');
    switch (node->type) {
        case NodeBinOp:
            expression_to_buf(buf, node->binop.left);
            switch (node->binop.type) {
                case OpSub: buf_write_char(buf,'-'); break;
                case OpAdd: buf_write_char(buf,'+'); break;
                case OpMlt: buf_write_char(buf,'*'); break;
                case OpDiv: buf_write_char(buf,'/'); break;
                case OpLe:  buf_write_cstr(buf, "<="); break;
                case OpGe:  buf_write_cstr(buf, ">="); break;
                case OpLt:  buf_write_cstr(buf, "<"); break;
                case OpGt:  buf_write_cstr(buf, ">"); break;
                case OpEq:  buf_write_cstr(buf, "=="); break;
                case OpNeq: buf_write_cstr(buf, "!="); break;
            }
            expression_to_buf(buf, node->binop.right);
            break;
        case NodeUnary:
            switch (node->unary.type) {
                case UnRef:         buf_write_char(buf, '&'); break;
                case UnDeref:       buf_write_char(buf, '*'); break;
                case UnNegative:    buf_write_char(buf, '-'); break;
                case UnNot:         buf_write_char(buf, '!'); break;
                case UnCompliment:  buf_write_char(buf, '~'); break;
            }
            expression_to_buf(buf, node->unary.target);
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
        default: return NULL;
    }
    buf_write_char(buf, ')');
    return *buf;
}
char* gen_c(ParserCtx* pctx, char** buf, Node* node) {
    info("%s", node_type_to_string(node->type));
    // size_t buf_size = 32*1024;
    // char original_buf[buf_size];
    // memset(original_buf, 0, buf_size);
    // char* buf = original_buf;
    switch (node->type) {
        case NodeVarDec:
           dbg("%zu", buf_write_c_type(buf, node->var_dec.type));
           dbg("%zu", buf_write_char(buf, ' '));
           dbg("%zu", buf_write_name(buf, node->var_dec.name));
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
            buf_write_c_type(buf, node->fn_dec.return_type->type_data);
            buf_write_char(buf, ' ');
            buf_write_name(buf, node->fn_dec.name);
            fflush(stdout);
            buf_write_char(buf, '(');
            for (size_t i = 0; i < node->fn_dec.args_count; i++) {
                Node* arg = node->fn_dec.args[i];
                if (i > 0) buf_write_char(buf, ',');
                buf_write_c_type(buf, arg->arg.type->type_data);
                buf_write_char(buf, ' ');
                buf_write_name(buf, arg->arg.name);
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
        default:
            err("Invalid Node type %s", node_type_to_string(node->type));
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
            "#include <stdint.h>\n"
            "typedef uint8_t    u8;\n"
            "typedef uint16_t   u16;\n"
            "typedef uint32_t   u32;\n"
            "typedef uint64_t   u64;\n"
            // "typedef uint128_t  u128;\n"
            "typedef int8_t     i8;\n"
            "typedef int16_t    i16;\n"
            "typedef int32_t    i32;\n"
            "typedef int64_t    i64;\n"
            // "typedef int128_t   i128;\n"
            "#include <stdio.h>\n");
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
    fclose(f);
    system("echo \"Output file:\"");
    // system("cat gala.out.c");
    system("clang -o prog gala.out.c");
    int ret = system("./prog");

    if (ret == -1) {
        perror("system");
    } else {
        int exit_code = WEXITSTATUS(ret);
        printf("Exit code: %d\n", exit_code);
    }

    return 1;
    system("rm ./gala.out.c");
    return 1;
}

