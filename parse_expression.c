#include "constants.h"
#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include "parse_number.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

Node* make_node_list(Parser* p, Node** nodes, int count) {
    Node* n = new_node(p);
    n->kind = NodeNodeList;
    n->node_list.count = count;
    n->node_list.nodes = arena_alloc(&p->arena, count * sizeof(Node*));
    memcpy(n->node_list.nodes, nodes, count * sizeof(Node*));
    return n;
}

Node* parse_condition(Parser*p) {
    Node* n = parse_expression(p);
    return n;
}
int is_lvalue(Node* lvalue) {
    if (!lvalue) return 0;
    if (0
            // || lvalue->kind == NodeVar
            || lvalue->kind == NodeSymbol
            || lvalue->kind == NodeModuleAccess
            || lvalue->kind == NodeIndex
            || lvalue->kind == NodeFieldAccess
       ) return 1;
    if (lvalue->kind == NodeUnary && lvalue->unary.type == UnDeref) return 1;
    dbg("%s not lvalue.", NodeKindToString(lvalue->kind));
    return 0;
}
OpType get_op(Token token) {
    switch (token.type) {
        case TokenPlus:        return OpAdd;
        case TokenMinus:       return OpSub;
        case TokenStar:        return OpMlt;
        case TokenSlash:       return OpDiv;
        case TokenPercent:     return OpMod;

        case TokenPipe:        return OpOr;
        case TokenCaret:       return OpXor;
        case TokenAmpersand:   return OpAnd;

        case TokenOrOr:    return OpOrOr;
        case TokenAndAnd:      return OpAndAnd;

        case TokenEqual:  return OpEq;
        case TokenNotEqual:   return OpNeq;

        case TokenLess:        return OpLt;
        case TokenGreater:     return OpGt;
        case TokenLessEqual:   return OpLe;
        case TokenGreaterEqual:return OpGe;

        case TokenShiftL:      return OpLSh;
        case TokenShiftR:      return OpRSh;

        case TokenAssign:      return OpAssign;

        default:
                               return OpNone;
    }
}


// binpo
// expr = term { op term } is left  associative: ( a + b ) + c
// expr = term [ op expr ] is right associative: a + ( b + c )
// assignment, casts, unary, (exponantiation if present) and conditional
// are right associative (if next == required)
// the rest are left associative (while next == required)
// structs: ident.{abc}
Node* parse_primary(Parser *p) {
    if (current(p).type == TokenIdent) {
        Node* path =  parse_path(p);
        if (!path) {
            panic("Failed to parse path.");
            return 0;
        }
        // struct
        if (current(p).type == TokenDot &&
                peek(p).type == TokenOpenBrace) {
            Token start = consume(p); // "."
            Token open = consume(p); // "{"
            // name ":" value ","
            Node* decs[10];
            int count = 0;
            while (current(p).type == TokenIdent) {
                Node* ident = parse_symbol(p);
                if (!ident) {
                    panic("Expected identifier for struct literal field.");
                    return 0;
                }
                if (current(p).type != TokenColon) {
                    panic("Need colon.");
                }
                consume(p);
                Node* expr = parse_expression(p);
                if (!expr) {
                    panic("Failed to parse expression.");
                    return NULL;
                }
                Node* n = new_node(p);
                n->kind = NodeNamedField;
                n->named_field.ident = ident;
                n->named_field.expr = expr;
                decs[count++] = n;
                if (current(p).type == TokenCloseBrace) {
                    // consume(p); // "}"
                    break;
                }
                expect(p, TokenComma);
                consume(p); // ","
            }
            expect(p, TokenCloseBrace);
            consume(p); // "}"
            Node* n = new_node(p);
            if (!n) {
                panic("Failed to allocate new node.");
            }
            n->kind = NodeStructLit;
            n->token = start;
            n->struct_literal.fields = make_node_list(p, decs, count);
            n->struct_literal.fields->token = start;
            n->struct_literal.type_name = path;
            return n;
        }
        return path;
    } else if (current(p).type == TokenString) {
        Token str = consume(p);
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->token = str;
        n->string_literal = str.ident;
        return n;
    } else if (current(p).type == TokenNumber) {
        Token num = consume(p);
        dbg("Number at %d %d", num.line, num.col);
        ParsedNumber out = {0};
        if (!parse_number(num.ident.name, num.ident.length, &out)) {
            panic("Failed to parse number numeric value.");
            return NULL;
        }
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeNumLit;
        n->token = num;

        n->number.kind = out.kind;
        if (out.kind == NumKindInt)
            n->number.integer = out.i;
        if (out.kind == NumKindFloat)
            n->number.number = out.f;
        n->number.str_repr = num.ident;
        return n;
    } else if (current(p).type == TokenOpenParen) {
        consume(p); // "("
        Node* expr = parse_expression(p);
        if (!expr) {
            err("Failed to parse expression.");
        }
        if (current(p).type != TokenCloseParen) {
            // try not to consume
            err("Expected \")\" got %s.", get_token_data(current(p)));
            return NULL;
        }
        consume(p); // ")"
        return expr;
    }
    err("failed to parse primary, got %s", get_token_data(current(p)));
    return NULL;
} // ident | number | ( expr )
Node* parse_postfix(Parser *p) {
    Node* primary = parse_primary(p);
    while(current(p).type == TokenOpenParen ||
            current(p).type == TokenOpenSquare || 
            current(p).type == TokenDot) {
        if (current(p).type == TokenOpenParen) { // fn call
            Token start = consume(p); // "("
            // fn call has args
            int cap = 10, count = 0;
            Node** args = calloc(1, cap*sizeof(Node*));
            while (current(p).type != TokenCloseParen) {
                Node* expr = parse_expression(p);
                if (!expr) {
                    panic("Failed to parse expression in arg.");
                    return 0;
                }
                if (count >= cap) {
                    cap *= 2;
                    args = realloc(args, cap*sizeof(Node*));
                }
                args[count++] = expr;
                if (current(p).type == TokenComma) {
                    consume(p); // ","
                } else {
                    break;
                }
            }
            // must be ")" cuz of loop condition
            consume(p); // ")"
            Node* fn_args = 0;
            if (count > 0) {
                Node** arena_args = NULL;
                arena_args = arena_alloc(&p->arena, count*sizeof(Node*));
                if (!arena_args) {
                    panic("Failed to allocate memory in arena for args.");
                    return 0;
                }
                memcpy(arena_args, args, count*sizeof(Node*));
                fn_args = new_node(p);
                if (!fn_args) {
                    panic("Failed to allocate memory for new node.");
                    return 0;
                }
                fn_args->kind = NodeNodeList;
                fn_args->node_list.nodes = arena_args;
                fn_args->node_list.count = count;
            }
            Node* fn_call = new_node(p);
            if (!fn_call) {
                err("Failed to allocate new node.");
                return NULL;;
            }
            fn_call->kind = NodeFnCall;
            fn_call->token = start;
            fn_call->fn_call.args = fn_args;
            // TODO make sure it is a identifier (var node in this case);
            fn_call->fn_call.target = primary;
            primary = fn_call;
        } else if (current(p).type == TokenOpenSquare) {
            Token paren = consume(p);
            Node* expr  = parse_expression(p);
            if (!expr) {
                err("Failed to parse expression (array index).");
                return NULL;
            }
            if (current(p).type != TokenCloseSquare) {
                err("Expected \"]\" after array index, got %s.",
                        get_token_data(current(p)));
                return  NULL;
            }
            consume(p); // "]"
            Node* n = new_node(p);
            if (!n) {
                panic("Failed to allocate memory.");
                return NULL;
            }
            n->kind = NodeIndex;
            n->token = paren;
            n->index.target = primary;
            n->index.index = expr;
            primary = n; // set primary (to return) to this
        } else if (current(p).type == TokenDot) {
            Token dot = consume(p); // "."
            if (current(p).type != TokenIdent) {
                err("Expected identifier, got %s.", get_token_data(current(p)));
                return NULL;
            }
            Token ident = consume(p);
            Node* n = new_node(p);
            if (!n) {
                panic("Failed to allocate memory.");
                return NULL;
            }
            n->kind = NodeFieldAccess;
            n->token = dot;
            n->field_access.target = primary;
            n->field_access.field_name = ident.ident;
            primary = n; // set primary (to return) to this
        }
    }
    return primary;
}
//
    // posfix as type
Node* parse_unary(Parser *p) {
    Token op = current(p);
    if (    op.type == TokenStar
        ||  op.type == TokenAmpersand
        ||  op.type == TokenMinus
        ||  op.type == TokenBang
        ||  op.type == TokenTilde) {
        Token op = consume(p); // op "*" | "&" | "-" | "!" | "~"
        Node* target = parse_unary(p);
        if (!target) {
            err("Failed to parse unary expression.");
            return NULL;
        }
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->token = op;
        n->kind = NodeUnary;
        switch (op.type) {
            case TokenStar:         n->unary.type = UnDeref; break;
            case TokenAmpersand:    n->unary.type = UnRef; break;
            case TokenMinus:        n->unary.type = UnNegative; break;
            case TokenBang:         n->unary.type = UnNot; break;
            case TokenTilde:        n->unary.type = UnCompliment; break;
            default: break; // can't happen
        }
        n->unary.target = target;
        return n;
    } else {
        Node* cast = parse_postfix(p);
        if (!cast) {
            err("failed to parse cast expression.");
            return NULL;
        }
        return cast;
    }
} //  * & - ~ !
Node* parse_cast(Parser *p) { // reimplement
    Node* n = parse_unary(p);
    if (!n) {
        err("Failed to parse unary.");
        return NULL;
    }
    while (current(p).type == TokenKeyword
            && current(p).kw == KwAs) {
        Token _as = consume(p); // "as"
        Node* _type = parse_type(p);
        if (!_type) {
            err("Failed to parse type in cast.");
            return NULL;
        }
        Node* cast_n = new_node(p);
        if (!cast_n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        cast_n->kind = NodeCast;
        cast_n->token = _as;
        
        cast_n->cast.to = _type->type_data; // nodes persist
        cast_n->cast.target = n;
        n = cast_n;
    }

    return n;
} // (type) and what not
Node* parse_multiplicative(Parser *p) {
    Node* cast = parse_cast(p);
    if (!cast) {
        err("Failed to parse unary expression.");
        return NULL;
    }
    while (current(p).type == TokenStar
        || current(p).type == TokenSlash
        || current(p).type == TokenPercent) {
        Token op = consume(p);  // "*" | "/" | "%"
        Node* rhs_cast = parse_cast(p);
        if (! rhs_cast) {
            err("Failed to parse rhs unary expression.");
            return NULL;
        }
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;
        switch (op.type) {
            case TokenStar:         n->binop.type = OpMlt; break;
            case TokenSlash:        n->binop.type = OpDiv; break;
            case TokenPercent:      n->binop.type = OpMod; break;
            default: break; // can't happen
        }
        n->binop.left = cast;
        n->binop.right = rhs_cast;
        cast = n;
    }
    return cast;
} // * / %
Node* parse_additive(Parser *p) {
    Node* multiplicative = parse_multiplicative(p);
    if (!multiplicative) {
        err("Failed to parse multiplicative expression.");
        return NULL;
    }
    while (current(p).type == TokenPlus
        || current(p).type == TokenMinus) {
        Token op = consume(p);  // "+" | "-"
        Node* rhs_multiplicative = parse_multiplicative(p);
        if (!rhs_multiplicative) {
            err("Failed to parse rhs multiplicative expression.");
            return NULL;
        }
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;
        n->binop.type = op.type == TokenPlus ? OpAdd : OpSub;
        n->binop.left = multiplicative;
        n->binop.right = rhs_multiplicative;
        multiplicative = n;
    }
    return multiplicative;
} // + -
Node* parse_bit_shift(Parser *p) {
    Node* additive = parse_additive(p);
    if (!additive) {
        err("Failed to parse additive expression.");
        return NULL;
    }
    while (current(p).type == TokenShiftL
        || current(p).type == TokenShiftR) {
        Token op = consume(p);  // "<<" | ">>"
        Node* rhs_additive = parse_additive(p);
        if (!rhs_additive) {
            err("Failed to parse rhs additive expression.");
            return NULL;
        }
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        n->binop.type = op.type == TokenShiftL ? OpLSh : OpRSh;
        n->binop.left = additive;
        n->binop.right = rhs_additive;
        additive = n;
    }
    return additive;
} // << >>
Node* parse_relational_comp(Parser *p) {
    Node* bit_shift = parse_bit_shift(p);
    if (!bit_shift) {
        err("Failed to parse shift expression.");
        return NULL;
    }
    while (current(p).type == TokenLess
        || current(p).type == TokenGreater
        || current(p).type == TokenLessEqual
        || current(p).type == TokenGreaterEqual ) {
        Token op = consume(p);  // "<=" | ">=" | "<" | ">"
        Node* rhs_bit_shift = parse_bit_shift(p);
        if (!rhs_bit_shift) {
            err("Failed to parse rhs shift expression.");
            return NULL;
        }
                Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        switch (op.type) {
            case TokenLessEqual:    n->binop.type = OpLe; break;
            case TokenGreaterEqual: n->binop.type = OpGe; break;
            case TokenLess:         n->binop.type = OpLt; break;
            case TokenGreater:      n->binop.type = OpGt; break;
            default: break; // can't happen
        }
        n->binop.left = bit_shift;
        n->binop.right = rhs_bit_shift;
        bit_shift = n;
    }
    return bit_shift;
} // <= >= < >
Node* parse_logical_comp(Parser *p) {
    Node* relational_comp = parse_relational_comp(p);
    if (!relational_comp) {
        err("Failed to parse relational comparasion.");
        return NULL;
    }
    while (current(p).type == TokenEqual
        || current(p).type == TokenNotEqual) {
        Token op = consume(p); // "==" | "!="
        Node* rhs_relational_comp = parse_relational_comp(p);
        if (!rhs_relational_comp) {
            err("Failed to parse rhs relational comp.");
            return NULL;
        }
                Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        n->binop.type = op.type == TokenEqual ? OpEq : OpNeq;
        n->binop.left = relational_comp;
        n->binop.right = rhs_relational_comp;
        relational_comp = n;
    }

    return relational_comp;
}
Node* parse_bitwise_and(Parser *p) {
    Node* logical_comp = parse_logical_comp(p);
    if (!logical_comp) {
        err("Failed to parse logical comparasion.");
        return NULL;
    }
    while (current(p).type == TokenAmpersand ) {
        Token op = consume(p); // "&" | "|"
        Node* rhs_locical_and = parse_logical_comp(p);
        if (!rhs_locical_and) {
            err("Failed to parse rhs logical comp.");
            return NULL;
        }
                Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        n->binop.type = OpAnd;
        n->binop.left = logical_comp;
        n->binop.right = rhs_locical_and;
        logical_comp = n;
    }

    return logical_comp;
}
Node* parse_bitwise_xor(Parser *p) {
    Node* bw_and = parse_bitwise_and(p);
    if (!bw_and) {
        err("Failed to parse bitwise and.");
        return NULL;
    }
    while (current(p).type == TokenCaret) {
        Token op = consume(p); // "^"
        Node* rhs_bw_and = parse_bitwise_and(p);
        if (!rhs_bw_and) {
            err("Failed to parse rhs bitwise and.");
            return NULL;
        }
                Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        n->binop.type = OpXor;
        n->binop.left = bw_and;
        n->binop.right = rhs_bw_and;
        bw_and = n;
    }

    return bw_and;
}
Node* parse_bitwise_or(Parser *p) {
    Node* bw_xor = parse_bitwise_xor(p);
    if (!bw_xor) {
        err("Failed to parse bitwise xor.");
        return NULL;
    }
    while (current(p).type == TokenPipe) {
        Token op = consume(p); // "|"
        Node* rhs_bw_xor = parse_bitwise_xor(p);
        if (!rhs_bw_xor) {
            err("Failed to parse rhs bitwise xor.");
            return NULL;
        }
                Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        n->binop.type = OpOr;
        n->binop.left = bw_xor;
        n->binop.right = rhs_bw_xor;
        bw_xor = n;
    }

    return bw_xor;
}

Node* parse_logical_and(Parser *p) {
    Node* bw_or = parse_bitwise_or(p);
    if (!bw_or) {
        err("Failed to parse bitwise or.");
        return NULL;
    }
    while (current(p).type == TokenAndAnd) {
        Token op = consume(p); // "&&"
        Node* rhs_bw_or = parse_bitwise_or(p);
        if (!rhs_bw_or) {
            err("Failed to parse rhs bitwise or.");
            return NULL;
        }
                Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        n->binop.type = OpAndAnd;
        n->binop.left = bw_or;
        n->binop.right = rhs_bw_or;
        bw_or = n;
    }

    return bw_or;
}
Node* parse_logical_or(Parser *p) {
    Node* logical_and = parse_logical_and(p);
    if (!logical_and) {
        err("Failed to parse logical and.");
        return NULL;
    }
    while (current(p).type == TokenOrOr) {
        Token op = consume(p); // "||"
        Node* rhs_locical_and = parse_logical_and(p);
        if (!rhs_locical_and) {
            err("Failed to parse rhs logical and.");
            return NULL;
        }
                Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp;
        n->token = op;

        n->binop.type = OpOrOr;
        n->binop.left = logical_and;
        n->binop.right = rhs_locical_and;
        logical_and = n;
    }

    return logical_and;
}
// no conditionals
Node* parse_conditional(Parser *p) {
    Node* logical_or = parse_logical_or(p);
    if (!logical_or) {
        err("Failed to parse logical or.");
        return NULL;
    }
    return logical_or;
}
Node* parse_assignment(Parser *p) {
    Node* lvalue = parse_conditional(p);
    if (!lvalue) {
        err("Failed to parse assignment.");
        return NULL;
    }
    
    if (current(p).type == TokenAssign) {
        if (!is_lvalue(lvalue)) {
            err("expression is not an lvalue.");
            return NULL;
        }
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeBinOp; // binop
        n->binop.type = OpAssign;
        n->token = consume(p); // "="
        
        Node* rhs_assignment = parse_assignment(p);
        if (!rhs_assignment) {
            err("Failed to parse rhs assignment.");
            return NULL;
        }
        n->binop.left = lvalue;
        n->binop.right = rhs_assignment;
        lvalue = n;
    } else if (current(p).type == TokenColon
            || current(p).type == TokenColonEqual
            || current(p).type == TokenDoubleColon) {
        warn("VARDEC");
        if (lvalue->kind != NodeSymbol) {
            err("lvalue MUST be a symbol for var declaration.");
            return NULL;
        }
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->kind = NodeVarDec;
        n->token = lvalue->token;
        n->var_dec.ident = lvalue; // set symbol
        if (current(p).type == TokenColon) {
            Token colon = consume(p);
            // parse type
            Node* type = parse_type(p);
            if (!type) {
                panic("Failed to parse type.");
                return NULL;
            }
            n->var_dec.type = type;
            switch (current(p).type) {
                case TokenAssign: /* variable */
                    n->kind = NodeVarDec;
                    n->var_dec.is_const = 0;
                    break;
                case TokenColon: /* const */
                    n->kind = NodeConstDec;
                    n->var_dec.is_const = 1;
                    break;
                case TokenSemicolon: /* no value, just vardec */
                    n->kind = NodeVarDec;
                    n->var_dec.is_const = 0;
                    n->var_dec.value = NULL;
                    break;
                default:
                    err("Expected \"=\" (or \":\" for constants), "
                            "got %s.", get_token_data(current(p)));
                    return NULL;
            }
            consume(p); // "="/":"
            Node* expr_n = parse_expression(p);
            if (!expr_n) {
                err("Failed to parse expression.");
                return NULL;
            }
            n->var_dec.value = expr_n;
        } else if (current(p).type == TokenDoubleColon) {
            // parse constant?
            TODO("handle ::");
        } else if (current(p).type == TokenColonEqual) {
            // inference
            consume(p); // ":="
            Node* expr_n = parse_expression(p);
            if (!expr_n) {
                err("Failed to parse expression.");
                return NULL;
            }
            n->var_dec.value = expr_n;
        } else {
            panic("Expected \":\" or \"::\" (for constants) for variable "
                    "declaration.");
        }
        lvalue = n; // set and return

    }

    return lvalue;
}
Node* parse_expression(Parser *p) {
    /* if (current(p).type == TokenKeyword) {
       if (current(p).kw == KwFn) {
       return parse_fn_body(p);
       }
       } */
    Node* assignment = parse_assignment(p);
    if (!assignment) {
        err("Failed to parse assignment.");
        return NULL;
    }
    // no comma ig. for args ofc.
    return assignment;

    // info("Current after assignemtn: %s", get_token_data(current(p)));
    while (current(p).type == TokenComma) {
        // info("binop??");
        Node* n = new_node(p);
        if (!n) {
            panic("Failed to allocate memory.");
            return NULL;
        }
        n->token = consume(p);

        Node* rhs_assignment = parse_assignment(p);
        if (!rhs_assignment) {
            err("Failed to parse rhs assignment.");
            return NULL;
        }
        n->binop.left = assignment;
        n->binop.right = rhs_assignment;
        assignment = n;
    }

    return assignment;
}
