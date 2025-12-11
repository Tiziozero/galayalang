#ifndef TOKEN_H
#define TOKEN_H
#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gala.h>

typedef struct Name Name;
struct Name {
    char* name;
    usize length;
    union {
        struct {
            Name* type;
        } var_data;
    };
} ;
typedef enum {
    TOKEN_EOF,
    TOKEN_IDENT,
    TOKEN_O_BRAC,     // (
    TOKEN_C_BRAC,     // )
    TOKEN_O_CBRAC,    // {
    TOKEN_C_CBRAC,    // }
    TOKEN_O_SBRAC,    // [
    TOKEN_C_SBRAC,    // ]
    TOKEN_OP_PLUS,    // +
    TOKEN_OP_MINUS,   // -
    TOKEN_F_SLASH,    // /
    TOKEN_B_SLASH,    // \

    TOKEN_STAR,       // *
    TOKEN_COMMA,      // ,
    TOKEN_SEMI,       // ;
    TOKEN_KW,
    TOKEN_NUM,
    TOKEN_CHAR,
    TOKEN_STRING,
    TOKEN_AMPERSAND,  // &
    TOKEN_PNKT, // punctuation: &, ",", *, |, \,...
    TOKEN_PREPROC,
    TOKEN_COMMENT,
    TOKEN_EQUAL,
} TokenType;

inline static const char* get_token_type_name(TokenType t) {
    switch (t) {
        case TOKEN_EOF: return "TOKEN_EOF";
        case TOKEN_IDENT: return "TOKEN_IDENT";
        case TOKEN_O_BRAC: return "TOKEN_O_BRAC";
        case TOKEN_C_BRAC: return "TOKEN_C_BRAC";
        case TOKEN_O_CBRAC: return "TOKEN_O_CBRAC";
        case TOKEN_C_CBRAC: return "TOKEN_C_CBRAC";
        case TOKEN_O_SBRAC: return "TOKEN_O_SBRAC";
        case TOKEN_C_SBRAC: return "TOKEN_C_SBRAC";
        case TOKEN_OP_PLUS: return "TOKEN_OP_PLUS";
        case TOKEN_OP_MINUS: return "TOKEN_OP_MINUS";
        case TOKEN_F_SLASH: return "TOKEN_F_SLASH";
        case TOKEN_B_SLASH: return "TOKEN_B_SLASH";
        case TOKEN_STAR: return "TOKEN_STAR";
        case TOKEN_COMMA: return "TOKEN_COMMA";
        case TOKEN_SEMI: return "TOKEN_SEMI";
        case TOKEN_KW: return "TOKEN_KW";
        case TOKEN_NUM: return "TOKEN_NUM";
        case TOKEN_CHAR: return "TOKEN_CHAR";
        case TOKEN_STRING: return "TOKEN_STRING";
        case TOKEN_PNKT: return "TOKEN_PNKT";
        case TOKEN_PREPROC: return "TOKEN_PREPROC";
        case TOKEN_COMMENT: return "TOKEN_COMMENT";
        case TOKEN_EQUAL: return "TOKEN_EQUAL";
        case TOKEN_AMPERSAND: return "TOKEN_AMPERSAND";
        // default: FAILED("UNKNOWN");
        }
    FAILED("UNKNOWN");
    return NULL;
}
typedef struct {
    TokenType type;
    Name name;
    union {
        Name number; // idk just to differentiate
        usize a;
    };
} Token;
#endif // TOKEN_H
