#ifndef USER_MSGS_H
#define USER_MSGS_H
#include "logger.h"
#include "parser.h"




static inline char* type_to_human_str(char* buf, size_t size, Type* type) {
    if (!buf) {
        panic("no buffer.");
        return (char*)"<null type>";
    }
    if (!type) {
        panic("Null type got.");
        return (char*)"<null type>";
    }
    if (type->kind == tt_none) {
        panic("type type is none (tt_none)");
        return (char*)"<null type>";
    }
    memset(buf, 0, size); // set rest of buffer to 0;
    if (type->kind == tt_ptr) {
        if (!type->ptr) {
            panic("pointer has no type.");
            return(char*)("<pointer with no type?>");
        }
        *buf = '*';
        buf++;
        type_to_human_str(buf, size - 1, type->ptr);
    } else {
        memcpy(buf, type->name.name, type->name.length > size
                ? size:type->name.length);
    }

    return buf;
}
static inline void usr_warn(const char *fmt, ...) {
    char buf[1024];
    va_list args;
    va_start(args, fmt);
    int len = format_log(
        buf, sizeof(buf),
        "<{[ WARNING!!11! ]}>",
        COLOR_WARN,
        fmt,
        args);
    va_end(args);

    if (len > 0) write_log(2, buf, (size_t)len);
}

static inline char* get_humane_node_name(Node* node) {
    switch (node->kind) { // only nodes I want the user to know
        case NodeVarDec: return (char*)"Variable Declaration";
        case NodeVar: return (char*)"Variable";
        case NodeBinOp: return (char*)"Binary Operation";
        case NodeNumLit: return (char*)"Number Literal";
        case NodeUnary: return (char*)"Unary";
        default: panic("Unhandeled node name to humanise %s",node_type_to_string(node->kind));
    }
    panic("Invalid node to humanise name of.");
    return NULL;
}
static inline void print_node_data(Node* node, int indent) {
    TODO("Implement");
}
#define TAB4 "    "

static inline void highlight_code_problem(ParserCtx* pctx, char* str, int len,
        Token* token, Token* end_token) {
    if (!str) return;
    if (!token) return;
    printf(TAB4); // print to indent with "[ERROR ] "

    // use "    " instead of \t
    printf("%s on line %zu:%zu:\n" TAB4,str, token->line, token->col);
    printf(TAB4); // print to indent with "[ERROR ] "
    
    // lines start at 1 cus user stuupid
    if (token->line - 1 > pctx->lexer->lines_count) panic("Line out of file.");
    char* line = pctx->lexer->lines[token->line-1];
    if (!line) panic("Failed to get line");
    if (token->col - 1 > strlen(line)) panic("char out of line");
    printf("\"%s\"\n"TAB4, line);
    printf(TAB4); // print to indent with "[ERROR ] "

    for (size_t i = 0; i < token->col ; i++) printf(" ");
    printf("^\n");
}
static inline void usr_error(ParserCtx* pctx, char* msg, Node* node) {
    print_format_start(BOLD, RED);
    printf("error: " RESET "%s\n", msg);
    if (!node) return;

    highlight_code_problem(pctx, get_humane_node_name(node),
            strlen(get_humane_node_name(node)), &node->token, NULL);
}

#endif // USER_MSGS_H
