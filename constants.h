#ifndef CONSTANTS_H
#define CONSTANTS_H
typedef enum {
    UnNone = 0,
    UnRef,
    UnDeref,
    UnNegative,
    UnCompliment, // "~": 0b1010 -> 0b0101
    UnNot,
} UnaryType;
typedef enum {
    OpNone = 0,

    // assignment (right-associative)
    OpAssign,

    // logical
    OpOrOr,     // ||
    OpAndAnd,   // &&

    // bitwise
    OpOr,       // |
    OpXor,      // ^
    OpAnd,      // &

    // equality
    OpEq,       // ==
    OpNeq,      // !=

    // relational
    OpLt,       // <
    OpGt,       // >
    OpLe,       // <=
    OpGe,       // >=

    // shifts
    OpLSh,      // <<
    OpRSh,      // >>

    // additive
    OpAdd,      // +
    OpSub,      // -

    // multiplicative
    OpMlt,      // *
    OpDiv,      // /
    OpMod,      // %
    OpComma,    // ,
    OpAddAssign,
    OpSubAssign,
    OpMltAssign,
    OpDivAssign,
    OpModAssign,

    OpAndAssign,
    OpOrAssign, 
    OpXorAssign,
    OpLShAssign,
    OpRShAssign,
} OpType;
#endif // CONSTANTS_H
