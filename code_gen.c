#include "code_gen.h"
#include "parser.h"
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

char* as_c_print_type(char* buf, Type t) {
    if (t.type == tt_ptr) {
        buf = as_c_print_type(buf, *t.ptr);
        *(buf++) = '*';
        return buf;
    } else if (t.type == tt_ptr) { // do the same as ptr
                                   // and pray for a not out of bound
        buf = as_c_print_type(buf, *t.ptr);
        *(buf++) = '*';
        return buf;
    }
    memcpy(buf, t.name.name, t.name.length);
    buf += t.name.length;
    return buf;
}


char* expression_to_buf(char* buf, Node* node) {
    *(buf++) = '(';
    switch (node->type) {
        case NodeBinOp:
            buf = expression_to_buf(buf, node->binop.left);
            switch (node->binop.type) {
                case OpSub: *(buf++) = '-'; break;
                case OpAdd: *(buf++) = '+'; break;
                case OpMlt: *(buf++) = '*'; break;
                case OpDiv: *(buf++) = '/'; break;
            }
            buf = expression_to_buf(buf, node->binop.right);
        case NodeUnary:
            switch (node->unary.type) {
                case UnRef:         *(buf++) = '&'; break;
                case UnDeref:       *(buf++) = '*'; break;
                case UnNegative:    *(buf++) = '-'; break;
                case UnNot:         *(buf++) = '!'; break;
                case UnCompliment:  *(buf++) = '~'; break;
            }
            break;
        case NodeNumLit:
            memcpy(buf, node->number.str_repr.name,
                    node->number.str_repr.length);
            buf += node-> number.str_repr.length;
            break;
        case NodeVar:
            memcpy(buf, node->var.name.name,
                    node->var.name.length);
            buf += node-> var.name.length;
            break;
        case NodeFnCall:
            memcpy(buf, node->fn_call.fn_name.name,
                    node->fn_call.fn_name.length);
            buf += node-> fn_call.fn_name.length;
            *(buf++) = '(';
            for (size_t i = 0; i < node->fn_call.args_count; i++) {
                if (i > 0) *(buf++) = ','; // comma for next arg
                buf = expression_to_buf(buf, node->fn_call.args[i]);
            }
            *(buf++) = ')';
            break;
        default: return NULL;
    }
    *(buf++) = ')';
    return buf;
}
int gen_c(ParserCtx* pctx, FILE* f, Node* node) {
    info("%s", node_type_to_string(node->type));
    char buf[32*1024];
    memset(buf, 0, sizeof(buf));
    char* print_buf = buf;
    switch (node->type) {
        case NodeVarDec:
            print_buf = as_c_print_type(print_buf, node->var_dec.type);
            *(print_buf++) = ' ';
            memcpy(print_buf, node->var_dec.name.name,
                    node->var_dec.name.length);
            print_buf += node->var_dec.name.length;
            if (!node->var_dec.value) {
                *(print_buf++) = ';';
                // TODO("implement");
            } else {
                *(print_buf++) = ' ';
                *(print_buf++) = '=';
                *(print_buf++) = ' ';
                print_buf = expression_to_buf(print_buf, node->var_dec.value);
                *(print_buf++) = ';';
            }
            break;
        case NodeFnDec:
            print_buf = as_c_print_type(print_buf,
                    node->fn_dec.return_type->type_data);
            break;
        default:
            err("Invalid Node type %s", node_type_to_string(node->type));
            assert(0);
            return 0;
    }
    fprintf(f, "%s\n", buf);
    return 1;
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
            "#define u8     uint8_t\n"
            "#define u16    uint16_t\n"
            "#define u32    uint32_t\n"
            "#define u64    uint64_t\n"
            "#define u128   uint128_t\n"
            "#define i8     int8_t\n"
            "#define i16    int16_t\n"
            "#define i32    int32_t\n"
            "#define i64    int64_t\n"
            "#define i128   int128_t\n"
            "#include <stdio.h>\n");
    for (size_t i = 0; i < ast->nodes_count; i++) {
        Node* node = ast->nodes[i];
        if (!gen_c(pctx, f, node)) {
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

    return 1;
    system("rm ./gala.out.c");
    return 1;
}

