#ifndef PARSER_H
#define PARSER_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "lexer.h"
#include "utils.h"
#ifndef PTR_SIZE
#define PTR_SIZE sizeof(void*)
#endif

typedef struct ProgramState ProgramState;
typedef struct Node Node;
typedef struct Type Type;
typedef struct Parser Parser;
typedef struct Symbol Symbol;
typedef enum {
    NodeNone,
    // symbol related shi
    NodeSymbol,
    NodeVarDec, // both fns and vars
    NodeTypeData, // symbol stuff ig

    // expression shi
    NodeCount, // counut
} NodeKind;
typedef enum {
    tt_to_determinate = 0,
    tt_fn,
    tt_u8,
    tt_u16,
    tt_u32,
    tt_u64,
    tt_u128,
    tt_i8,
    tt_i16,
    tt_i32,
    tt_i64,
    tt_i128,
    tt_f32,
    tt_f64,
    tt_ptr,
    tt_usize,
    tt_char,
    tt_struct,
    tt_untyped_unsigned_int,
    tt_untyped_int, // could be either ig
    tt_untyped_float,
    tt_untyped_struct,
    tt_void,
} TypeKind;
struct Symbol {

};
struct Type {
    TypeKind kind;
    size_t size;
    Span name;
    Node* symbol; // path if module access, symbol if name. for parsing
    Type* alias;
    union {
        Type* ptr;
    };
};
struct Node {
    Token token;
    // Symbol symbl;
    NodeKind kind;
    union {
        //symbols stuf like vars and decs
        Span symbol;
        struct {
            Node* symbol;
            Node* type;
        } var_dec;
        Type* type_data;
        struct {
            Span target;
            Node* module;
        } module_access;
        // expression styff
    };
};



struct Parser {
    char*   path;
    Span    module_name;
    Span    module_code;
    Lexer*  l;
    size_t  tokens_count, tokens_index;
    Arena   arena;
    Node**  nodes;
    size_t  nodes_count;
    size_t  nodes_cap;
};
static const size_t ptr_size = PTR_SIZE;
Parser* pctx_new(Lexer* l, char* path);
Node* parse(Parser *p);
int parser_destry(Parser *p);
Span get_name_from_path(const char *path);


#endif // PARSER_H
