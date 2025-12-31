#include "code_gen.h"
#include "parser.h"
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
                    n->binop.right, name, n->binop.type == OpAssign ? 1 : 0))
            return 0;
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
int gen_c(FILE* f, Node node) {
    char  buf[1024];
    memset(buf, 0, 1024);
    switch (node.type) {
        case NodeVarDec: {
         as_c_print_type(buf, node.var_dec.type);
         if (node.var_dec.value == NULL) {
             fprintf(f, "%s %.*s;\n",
                     buf,
                     (int)node.var_dec.name.length,
                     node.var_dec.name.name);
         } else {
             struct Expr expr  =
                 get_expression_as_name(node.var_dec.value);
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

