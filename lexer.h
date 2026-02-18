#ifndef LEXER_H
#define LEXER_H
#include <stdint.h>
#include "utils.h"
#include "logger.h"

typedef enum {
    KwFn,
    KwAs,
    KwWhile,
    KwReturn,
    KwIf,
    KwElse,
    KwStruct,
    KwEnum,
    KwUnion,
    KwExtern,
    KwNone,
} KeyWord;
static Name key_words[] = {
    {.name="fn", .length=2},
    {.name="as", .length=2},
    {.name="while", .length=5},
    {.name="return", .length=6},
    {.name="if", .length=2},
    {.name="else", .length=4},
    {.name="struct", .length=6},
    {.name="enum", .length=4},
    {.name="union", .length=5},
    {.name="extern", .length=6},
};
typedef enum {
    TokenNone,
    // Identifiers / literals
    TokenIdent,
    TokenKeyword,
    TokenNumber,
    TokenString,

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
    TokenTilde,            // ~
    TokenColonEqual,        // :=

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
    char* chr;
    union {
        Name ident;
        KeyWord kw;
        Name number;
        Name string;
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
        case '~': return TokenTilde;

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
        case ':':
            if (c2 == '=') return TokenColonEqual;
    }

    return TokenEOF;
}



static inline const char* get_token_type(TokenType t) {
    switch (t) {
        case TokenString: return "TokenString"; break;
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
    char* code;
    size_t max_tokens;
    size_t tokens_count;
    Token* tokens;
    char** lines;
    size_t lines_count;
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
    if (l->tokens_count >= l->max_tokens) {
        l->max_tokens *= 2;
        // info("increasing token space to %zu...", l->max_tokens);
        l->tokens = (Token*)realloc(l->tokens, l->max_tokens*sizeof(Token));
        if (l->tokens == NULL) {
            err("Failed to reallocate memory for lexer.");
            exit(1);
            return 1;
        }
    }
    // info("Token Count: %zu. Max capacity: %zu.",
         // l->tokens_count, l->max_tokens);
    l->tokens[l->tokens_count++] = t;
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

/*

static inline int lexer_add_line(Lexer* lexer, char* line) {
    if (lexer->lines_count == 0) {
        memset(lexer->lines, 0, sizeof(lexer->lines));
    }
    if (lexer->lines_count >= 1000) panic("More lines neede.");

    lexer->lines[lexer->lines_count++] = line;
    return 1;
}
// a b c d e f g 
// handles new lines. increments i, updates col and line.
// after that it adds the line. it adds from "last_line" to this newline char
static inline void handle_new_line(Lexer* lexer, char* buf, size_t* i,
        size_t size, size_t* line, size_t* col, char* last_line) {
    char* cur = &buf[*i];
    if (c == '\n' || (buf[*i] == '\r' && buf[(*i)+1] == '\n')) {
        (*column) = 1;
        (*line)++;
        (*i)++;
        if (buf[(*i) - 1] == '\r' && buf[*i] == '\n') { // windows new line
            size_t line_size = cur - last_line - 1; 
            char* new_line = malloc(line_size + 1); // \0
            if (!new_line) panic("Failed malloc for new line");
            memset(new_line, 0, line_size); // assumes 1 byte per char
            memcpy(new_line, last_line, line_size); // this too
            lexer_add_line(lexer, new_line);
            (*i)++; // skip \n
        } else {
            size_t line_size = cur - last_line - 1; 
            char* new_line = malloc(line_size + 1); // \0
            if (!new_line) panic("Failed malloc for new line");
            memset(new_line, 0, line_size); // assumes 1 byte per char
            memcpy(new_line, last_line, line_size); // this too
            lexer_add_line(lexer, new_line);
        }
    }

}
*/

static inline Lexer* lexer(char* buf, size_t size) {
    dbg("lexing file...");
    Lexer* l = (Lexer*)malloc(sizeof(Lexer));
    l->max_tokens = 1024;
    l->tokens_count = 0;
    l->tokens  = (Token*)malloc(l->max_tokens*sizeof(Token));
    if (l->tokens == NULL) {
        err( "Failed to allocate memory for lexer.");
        exit(1);
        return NULL;
    }



    size_t i = 0;
    size_t line = 1;
    size_t column = 1;
    while (i < size) {
        char c = buf[i];
        size_t start_col = column;
        char* start_char = &buf[i];
        char peek = buf[i+1];

        if (c == '\n' || (buf[i] == '\r' && buf[i+1] == '\n')) {
            column = 1;
            line++;
            i++;
            if (buf[i] == '\n') // windows new line
                i++;
        } else if (c == '/' && peek == '/') {
            do i++; while (buf[i] != '\n' && !(buf[i] == '\r' && buf[i+1] == '\n'));
            i++; // '\n'
            if (buf[i] == '\n') // windows new line
                i++;
            line++;
            column = 1;
        } else if (c == ' ' || c == '\t') { // for now ig
            column++;
            i++;
        } else if (c == EOF) {
            lexer_add_token(l, (Token){TokenEOF, line, column, 0, 0});
            break;
        }else if(c == '"') {
            char* start = &buf[i];
            size_t col = 0;
            size_t len = 1;
            do { i++; column++; len++; }
            // end because new line bad
            while ((buf[i] != '"' || (buf[i] == '"' && buf[i-1] == '\\'))
                    && buf[i] != '\n'
                    && !(buf[i] == '\r' && buf[i+1] == '\n')); 
            if (c == '\n' || (buf[i] == '\r' && buf[i+1] == '\n'))
                {line++; column = 1;}
            if (buf[i] == '\n') // windows new line
                i++;
            Name string;
            string.name = start;
            string.length = len;
            Token t;
            memset(&t, 0, sizeof(Token));
            t.chr = start_char;
            t.type = TokenString;
            t.line = line;
            t.col = start_col;
            t.string = string;
            lexer_add_token(l, t);
            // to next char
            i++;
            
        }else if(c == '_' ||(c >= 'a' && c <= 'z')
                || (c >= 'A' && c <= 'Z') ) {
            const size_t col_start = column;
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
            if (is_keyword(n, key_words,
                        sizeof(key_words)/sizeof(key_words[0]))) {
                Token t;
                memset(&t, 0, sizeof(Token));
                t.chr = start_char;
                t.type = TokenKeyword;
                t.line = line;
                t.col = col_start;
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
                memset(&t, 0, sizeof(Token));
                t.chr = start_char;
                t.type = TokenIdent;
                t.line = line;
                t.col = col_start;
                t.ident = n;
                lexer_add_token(l, t);
            }
        } else if (c >= '0' && c <= '9') {
            char* name_start = &buf[i];
            size_t len = 0;
            size_t start_col = column;
            char cur = name_start[len];
            while((cur>= '0' && cur <= '9') || cur == '.') {
                 // increment all:i to next,len increses len,columnt for debug
                i++; len++; column++;
                cur = name_start[len];
            }
            // fwrite(name_start, 1, len, stdout);info(" of len: %zu\n", len);
            Name n = {.name=name_start, .length=len};
            Token t;
            memset(&t, 0, sizeof(Token));
            t.chr = start_char;
            t.type = TokenNumber;
            t.number = n;
            t.line=line;
            t.col=start_col;
            t.chr=start_char;
            lexer_add_token(l, t);
        } else if (is_double_symbol(c, peek) != TokenEOF) { // double symbols first
            lexer_add_token(l,(Token){
                .type=is_double_symbol(c, peek), .line=line,.col=start_col, .chr=start_char});
            column++;
            i++;i++; // eat two
        } else if (get_token_type_from_char(c) != TokenEOF) {
            lexer_add_token(l,(Token){
                .type=get_token_type_from_char(c), .line=line,.col=start_col,.chr=start_char});
            column++;
            i++;
        } else {
            err( "Unknown kakapoopoo: c (ascii %u) in %zu %zu",  c, line, start_col);
            free(l);
            return NULL;
        }
    }

    dbg("Finished lexical analysis.");

    return l;
}




#endif // LEXER_H
