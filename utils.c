#ifndef UTILS_H
#define UTILS_H
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
    TokenPipe,             // |
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

char* get_token_type(TokenType t) {
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
#endif // UTILS_C
