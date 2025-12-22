#ifndef LEXER_H
#define LEXER_H
#include <stdint.h>
#include "utils.h"
#include "logger.h"

typedef enum {
    KwFn,
    KwLet,
    KwWhile,
    KwReturn,
    KwIf,
    KwElse,
    KwStruct,
    KwNone,
} KeyWord;
static Name key_words[] = {
    {.name="fn", .length=2},
    {.name="let", .length=3},
    {.name="while", .length=5},
    {.name="return", .length=6},
    {.name="if", .length=2},
    {.name="else", .length=4},
    {.name="struct", .length=6},
};
typedef enum {
    // Identifiers / literals
    TokenIdent,
    TokenKeyword,
    TokenNumber,

    // Brackets
    TokenOpenParen,        // (
    TokenCloseParen,       // )
    TokenOpenSquare,       // [
    TokenCloseSquare,      // ]
    TokenOpenBrace,        // {
    TokenCloseBrace,       // }

    // Arithmetic
    TokenPlus,             // +
    TokenMinus,            // -
    TokenStar,             // *
    TokenSlash,            // /
    TokenPercent,          // %
    TokenCaret,            // ^
    TokenShiftL,           // <<
    TokenShiftR,           // >>

    // Assignment / comparison
    TokenAssign,           // =
    TokenEqual,            // ==
    TokenNotEqual,         // !=
    TokenLess,             // <
    TokenGreater,          // >
    TokenLessEqual,        // <=
    TokenGreaterEqual,     // >=

    // Logical / bitwise
    TokenAmpersand,        // &
    TokenAndAnd,           // &&
    TokenPipe,             // |
    TokenOrOr,             // ||
    TokenBang,             // !
    TokenQuestion,         // ?

    // Punctuation
    TokenDot,              // .
    TokenComma,            // ,
    TokenColon,            // :
    TokenSemicolon,        // ;
    TokenAt,               // @

    // Quotes
    TokenSingleQuote,      // '
    TokenDoubleQuote,      // "

    // Misc
    TokenBackslash,        // \

    TokenDollar,           // $

    // End
    TokenEOF,
} TokenType;

typedef struct {
    TokenType type;
    size_t line, col;
    union {
        Name ident;
        KeyWord kw;
        Name number;
    };
} Token;



static inline TokenType get_token_type_from_char(char c) {
    switch (c) {
        // Brackets
        case '(': return TokenOpenParen;
        case ')': return TokenCloseParen;
        case '[': return TokenOpenSquare;
        case ']': return TokenCloseSquare;
        case '{': return TokenOpenBrace;
        case '}': return TokenCloseBrace;

        // Arithmetic
        case '+': return TokenPlus;
        case '-': return TokenMinus;
        case '*': return TokenStar;
        case '/': return TokenSlash;
        case '%': return TokenPercent;
        case '^': return TokenCaret;

        // Assignment / comparison
        case '=': return TokenAssign;
        case '<': return TokenLess;
        case '>': return TokenGreater;

        // Logical / bitwise
        case '&': return TokenAmpersand;
        case '|': return TokenPipe;
        case '!': return TokenBang;
        case '?': return TokenQuestion;

        // Punctuation
        case '.': return TokenDot;
        case ',': return TokenComma;
        case ':': return TokenColon;
        case ';': return TokenSemicolon;
        case '@': return TokenAt;

        // Quotes
        case '\'': return TokenSingleQuote;
        case '"':  return TokenDoubleQuote;

        // Misc
        case '\\': return TokenBackslash;
        case '$':  return TokenDollar;

        default:
            return TokenEOF; // or TokenInvalid if you add one
    }
}


static inline TokenType is_double_symbol(char c1, char c2) {
    switch (c1) {
        case '=':
            if (c2 == '=') return TokenEqual;        // ==
            break;

        case '!':
            if (c2 == '=') return TokenNotEqual;     // !=
            break;

        case '<':
            if (c2 == '=') return TokenLessEqual;    // <=
            else if (c2 == '<') return TokenShiftL;    // <<
            break;

        case '>':
            if (c2 == '=') return TokenGreaterEqual; // >=
            else if (c2 == '>') return TokenShiftR;    // >>
            break;

        case '&':
            if (c2 == '&') return TokenAndAnd;       // &&
            break;

        case '|':
            if (c2 == '|') return TokenOrOr;         // ||
            break;
    }

    return TokenEOF;
}



static inline const char* get_token_type(TokenType t) {
    switch (t) {
        case TokenIdent: return "TokenIdent"; break;
        case TokenKeyword: return "TokenKeyword"; break;
        case TokenNumber: return "TokenNumber"; break;
        case TokenOpenParen: return "TokenOpenParen"; break;
        case TokenCloseParen: return "TokenCloseParen"; break;
        case TokenOpenSquare: return "TokenOpenSquare"; break;
        case TokenCloseSquare: return "TokenCloseSquare"; break;
        case TokenOpenBrace: return "TokenOpenBrace"; break;
        case TokenCloseBrace: return "TokenCloseBrace"; break;
        case TokenPlus: return "TokenPlus"; break;
        case TokenMinus: return "TokenMinus"; break;
        case TokenStar: return "TokenStar"; break;
        case TokenSlash: return "TokenSlash"; break;
        case TokenPercent: return "TokenPercent"; break;
        case TokenCaret: return "TokenCaret"; break;
        case TokenAssign: return "TokenAssign"; break;
        case TokenEqual: return "TokenEqual"; break;
        case TokenNotEqual: return "TokenNotEqual"; break;
        case TokenLess: return "TokenLess"; break;
        case TokenGreater: return "TokenGreater"; break;
        case TokenLessEqual: return "TokenLessEqual"; break;
        case TokenGreaterEqual: return "TokenGreaterEqual"; break;
        case TokenAmpersand: return "TokenAmpersand"; break;
        case TokenPipe: return "TokenPipe"; break;
        case TokenBang: return "TokenBang"; break;
        case TokenQuestion: return "TokenQuestion"; break;
        case TokenDot: return "TokenDot"; break;
        case TokenComma: return "TokenComma"; break;
        case TokenColon: return "TokenColon"; break;
        case TokenSemicolon: return "TokenSemicolon"; break;
        case TokenAt: return "TokenAt"; break;
        case TokenSingleQuote: return "TokenSingleQuote"; break;
        case TokenDoubleQuote: return "TokenDoubleQuote"; break;
        case TokenBackslash: return "TokenBackslash"; break;
        case TokenDollar: return "TokenDollar"; break;
        case TokenEOF: return "TokenEOF"; break;
        default: return "Unkown"; break;
    }

}


static inline const char* get_token_data(Token t) {
    static char s[100];
    snprintf(s, sizeof(s),
             "%s at %zu:%zu",
             get_token_type(t.type),
             t.line,
             t.col);
    return s;
}


typedef struct {
    size_t max_tokens;
    size_t tokens_count;
    Token* tokens;
} Lexer;

static inline Name get_keyword_name(KeyWord kw) {
    if (kw >= KwNone) return (Name){.name=0,.length=0};
    return key_words[kw];
}
static inline KeyWord get_name_kw(Name n) {
    for (uint8_t i = 0; i < KwNone; i++) {
        if (name_cmp(n, key_words[i])) {
            return (KeyWord)i;
        }
    }
    return KwNone;
}

static inline int lexer_add_token(Lexer* l, Token t) {
    l->tokens[l->tokens_count++] = t;
    if (l->tokens_count >= l->max_tokens) {
        l->max_tokens *= 2;
        l->tokens = (Token*)realloc(l->tokens, l->max_tokens);
        if (l->tokens == NULL) {
            err( "Failed to reallocate memory for lexer.");
            exit(1);
            return 1;
        }
    }
    return 0;
}

// returns 1 on true
static inline int is_keyword(Name n1, Name* kws, size_t kwlen) {
    for (int i = 0; i < kwlen; i++) {
        if (name_cmp(n1, kws[i])) {
            return 1;
        }
    }
    return 0;
}


static inline Lexer* lexer(char* buf, size_t size) {
    Lexer* l = (Lexer*)malloc(sizeof(Lexer));
    l->max_tokens = 1024;
    l->tokens_count = 0;
    l->tokens  = (Token*)malloc(1024*sizeof(l->max_tokens));
    if (l->tokens == NULL) {
        err( "Failed to allocate memory for lexer.");
        exit(1);
        return NULL;
    }



    size_t i = 0;
    size_t line = 0;
    size_t column = 0;

    while (i < size) {
        char c = buf[i];
        char peek = buf[i+1];

        if (c == '\n') {
            column = 0;
            line++;
            i++;
        } else if (c == '/' && peek == '/') {
            do i++; while (buf[i] != '\n');
            i++; // '\n'
            line++;
            column = 0;
        } else if (c == ' ' || c == '\t') {
            column++;
            i++;
        } else if (c == EOF) {
            lexer_add_token(l, (Token){TokenEOF, line, column});
            break;
        }else if(c == '_' ||(c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ) {
            char* name_start = &buf[i];
            size_t len = 0;
            char cur = name_start[len];
            while(cur=='_'||(cur >= 'a'&&cur<='z')||(cur>='A'&&cur<='Z')||
                (cur>= '0' && cur <= '9')) {
                i++; len++; column++; // increment all
                cur = name_start[len];
            }
            // fwrite(name_start, 1, len, stdout);info(" of len: %zu\n", len);
            Name n = {.name=name_start, .length=len};
            if(is_keyword(n,key_words,sizeof(key_words)/sizeof(key_words[0]))) {
                Token t;
                t.type = TokenKeyword;
                t.line = line;
                t.col = column;
                KeyWord kw = get_name_kw(n);
                if (kw == KwNone) {
                    n.name[n.length] = '\0';
                    err( "Failed to get name from name %s.", n.name);
                    return NULL;
                }
                t.kw = get_name_kw(n);
                lexer_add_token(l, t);
            } else {
                Token t;
                t.type = TokenIdent;
                t.line = line;
                t.col = column;
                t.ident = n;
                lexer_add_token(l, t);
            }
        } else if (c >= '0' && c <= '9') {
            char* name_start = &buf[i];
            size_t len = 0;
            char cur = name_start[len];
            while((cur>= '0' && cur <= '9') || cur == '.') {
                 // increment all:i to next,len increses len,columnt for debug
                i++; len++; column++;
                cur = name_start[len];
            }
            // fwrite(name_start, 1, len, stdout);info(" of len: %zu\n", len);
            Name n = {.name=name_start, .length=len};
            Token t;
            t.type = TokenNumber;
            t.number = n;
            lexer_add_token(l, t);
        } else if (is_double_symbol(c, peek) != TokenEOF) { // double symbols first
            lexer_add_token(l,(Token){
                .type=is_double_symbol(c, peek), .line=line,.col=column});
            column++;
            i++;i++; // eat two
        } else if (get_token_type_from_char(c) != TokenEOF) {
            lexer_add_token(l,(Token){
                .type=get_token_type_from_char(c), .line=line,.col=column});
            column++;
            i++;
        } else {
            err( "Unknown kakapoopoo: %c", c);
            free(l);
            return NULL;
        }
    }

    return l;
}




#endif // LEXER_H
