#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "type_checker.h"
#include "utils.h"
#include "parse_number.c"
#include "parser_get_type.c"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#define expected_got(expected, got) err("%d Exptected " expected \
        ", got: %s.", __LINE__, get_token_data(got)), pctx_fail(pctx), pr_fail();

void pctx_fail(ParserCtx* pctx) {
	err("Failed a stage in parsing.");
	assert(0);
}

int is_lvalue(Node* node) {
    return
    node->type == NodeUnary || // idk fix later maybe idk
    node->type == NodeVar || 
    node->type == NodeField || 
    node->type == NodeIndex ;
}
ParseRes parse_expression(ParserCtx* pctx);
ParseRes parse_assignment(ParserCtx* pctx);
ParseRes parse_top_level_statement(ParserCtx* pctx);
ParseRes parse_block_statement(ParserCtx* pctx);
ParseRes parse_type(ParserCtx* pctx);
// "*" / "["..."]"
ParseRes parse_type_prefix(ParserCtx* pctx) {
	if (current(pctx).type == TokenStar) {
		Node* n = new_node(pctx, NodeTypeData, consume(pctx));
		if (!n) {
			err("Failed to allocate new node,");
			return pr_fail();
		}
		n->type_data.type = tt_ptr;
		n->type_data.size = 0;
		n->type_data.name.name = 0;
		n->type_data.name.length = 0;
		n->type_data.name.length = 0;
		n->type_data.ptr = NULL;
		return pr_ok(n);
	} else if (current(pctx).type == TokenOpenSquare) {
		Node* n = new_node(pctx, NodeTypeData, consume(pctx));
		if (!n) {
			err("Failed to allocate new node,");
			return pr_fail();
		}
		Node* size = parse_expression(pctx).node; // sure, why not
		if (!size) {
			err("Failed to parse array size");
			return pr_fail();
		}
		int fails;
		if (!is_cmpt_constant(size)) {
			err("Array size must be a compile time constant");
		}
		if (current(pctx).type != TokenCloseSquare) {
			return expected_got("\"]\"", current(pctx));
		}
		consume(pctx); // "]"
		n->type_data.type = tt_array; // ptr.
		n->type_data.size = 0;
		n->type_data.name.name = 0;
		n->type_data.name.length = 0;
		n->type_data.static_array.size = size;
		n->type_data.static_array.type = NULL;
		return pr_ok(n);
	}
	return pr_fail();
}


ParseRes parse_type_atom(ParserCtx* pctx) {
	/*
	 * "(" type ")" type_postfix
	 */
	Node* type_data = NULL;
    Token start;
	if (current(pctx).type == TokenOpenParen) {
		start = consume(pctx); // "("
		type_data = parse_type(pctx).node;
		if (!type_data) {
			err("Failed to parse type data.");
			return pr_fail();
		}
		if (current(pctx).type != TokenCloseParen) {
			return expected_got("\")\" after type", current(pctx));
		}
		consume(pctx); // ")"
	} else if (current(pctx).type == TokenIdent) {
		type_data = new_node(pctx, NodeTypeData, consume(pctx));
		if (!type_data) {
			err("Failed to allocate new node.");
			return pr_fail();
		}
        start = type_data->token;
		type_data->type_data.type = tt_to_determinate;
		type_data->type_data.size = 0;
		type_data->type_data.name = type_data->token.ident;
	} else {
		err("Invalid toke type %s for type.", get_token_data(current(pctx)));
		return pr_fail();
	}
	if (!type_data) {
		err("what did u do bruh.");
		assert(0);
	}
    return pr_ok(type_data);
}

// will return type_data or fail
ParseRes parse_type(ParserCtx* pctx) {
    if (current(pctx).type == TokenStar
        || current(pctx).type == TokenOpenSquare) {
        Node* type_prefix = parse_type_prefix(pctx).node;
        if (!type_prefix) {
            err("Failed to parse type prefix.");
            return pr_fail();
        }
        Node* type = parse_type(pctx).node;
        if (!type) {
            err("Failed to parse type after postfix.");
            return pr_fail();
        }
        // set to pointer to typedata in node because nodes are valid
        // as long as pctx (and thus symbol store) is valid
        if (type_prefix->type_data.type == tt_ptr) {
            type_prefix->type_data.ptr = &type->type_data;
        } else {
            type_prefix->type_data.static_array.type = &type->type_data;
        }
        return pr_ok(type_prefix);
    } else {
        return parse_type_atom(pctx);
    }
}

ParserCtx* parse(Lexer* l) {
    ParserCtx* pctx = pctx_new(l->tokens, l->tokens_count);

    size_t i = 0;
    while (i < pctx->tokens_count && current(pctx).type != TokenEOF) {
        ParseRes pr = parse_top_level_statement(pctx);
        if (pr.ok == PrFail) {
            err("Failed to parse top level statement.");
            pctx_destry(pctx);
            return NULL;
        }
        if (pr.ok == PrOk) 
            ast_add_node(pctx->ast, pr.node);
        else if (pr.ok == PrMany) {
            for (size_t i = 0; i < pr.many.count; i++) {
                ast_add_node(pctx->ast, pr.many.nodes[i]);
            }
        }
    }

    int errs = 0;
    // symbols etc
    for (size_t i = 0; i < pctx->ast->nodes_count; i++) {
        if (!check_node_symbol(pctx, &pctx->symbols, pctx->ast->nodes[i])) {
            err("Invalid symbols in expression.");
            errs++;
        }
        if (!check_everythings_ok_with_types(pctx->ast->nodes[i])) {
            err("Invalid type after symbol check.");
            errs++;
            assert(0);
        }
    }
    if (errs > 0) {
        warn("errors in symbol check (%d errors).", errs);
        return NULL;
    }

    if (pctx->symbols.parent != NULL) {
        err("some fucker set symbol store.");
        assert(0);
    }
    // type check
    errs = 0;
    for (size_t i = 0; i < pctx->ast->nodes_count; i++) {
        if (!type_check_node(pctx, &pctx->symbols, pctx->ast->nodes[i])) {
            err("failed to type check expression.");
            errs++;
        }
    }
    if (errs > 0) {
        warn("errors in type check (%d errors).", errs);
        return NULL;
    }

    return pctx;
}


ParseRes parse_args(ParserCtx* pctx) {
    Node* nodes[10];
    memset(nodes, 0, sizeof(Node*) * 10);
    size_t count = 0;

    do {
        if (current(pctx).type == TokenComma && count > 0) consume(pctx);
        else if (current(pctx).type == TokenComma && count == 0)
            return err("cant have empty args."), pr_fail();

        // expression is a comma op which conflicts with args
        Node* expr = parse_assignment(pctx).node;
        if (!expr) {
            err("failed to parse argument");
            continue; // try to parse next one
        }
        nodes[count++] = expr;

    } while(current(pctx).type == TokenComma); // ";"

    if (count > 10 ) {
        err("More than 10 arguments found. max is 10 for now (hard coded)");
        count = 10;
    }

    // info("Parsed %zu arguments.", count);
    return pr_ok_many(nodes, count);
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

        case TokenShiftL:   return OpLSh;
        case TokenShiftR:  return OpRSh;

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

ParseRes parse_primary(ParserCtx* pctx) {
    if (current(pctx).type == TokenIdent) {
        Node* n = new_node(pctx, NodeVar, consume(pctx));
        if (!n) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        n->var.name = n->token.ident;
        return pr_ok(n);
    } else if (current(pctx).type == TokenNumber) {
        Token num = consume(pctx);
        double out;
        if (!parse_number(num.number.name, num.number.length, &out)) {
            err("Failed to parse number.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeNumLit, num);
        if (!n) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        n->number.number = out;
        n->number.str_repr = num.number;
        int has_dot = 0;
        for (size_t i = 0; i < n->number.str_repr.length; i++) {
            if (n->number.str_repr.name[i] == '.') has_dot = 1;
        }
        // set later in type checker
        // if (has_dot)
        //     n->number.type->name = (Name){.name="f32", .length=3};
        // else
        //     n->number.type->name = (Name){.name="u32", .length=3};

        return pr_ok(n);
    } else if (current(pctx).type == TokenOpenParen) {
        consume(pctx); // "("
        Node* expr = parse_expression(pctx).node;
        if (!expr) {
            err("Failed to parse expression.");
        }
        if (current(pctx).type != TokenCloseParen) {
            // try not to consume
            return expected_got("\")\" after epression", current(pctx));
        }
        consume(pctx); // ")"
        return pr_ok(expr);
    }
    err("failed to parse primary, got %s", get_token_data(current(pctx)));
    // print_name(current(pctx).string);
    return pr_fail();
} // ident | number | ( expr )
ParseRes parse_postfix(ParserCtx* pctx) {
    Node* primary = parse_primary(pctx).node;
    if (current(pctx).type == TokenOpenParen) { // fn call
        consume(pctx); // "("
        Node* fn_call = new_node(pctx, NodeFnCall, primary->token);
        if (!fn_call) {
            err("Failed to allocate new node.");
            return pr_fail();;
        }
        // TODO make sure it is a identifier (var node in this case);
        fn_call->fn_call.fn_name = primary->var.name;
        // fn call has args
        if (current(pctx).type != TokenCloseParen) {
            ParseRes args = parse_args(pctx);
            if (args.ok != PrMany) {
                err("Failed to parse args.");
                return pr_fail();
            }
            if (current(pctx).type != TokenCloseParen) {
                return expected_got("\")\" after function call", consume(pctx));
            }
            Node** args_ptr = arena_alloc(&pctx->gpa,
                                          args.many.count*sizeof(Node*));
            if (!args_ptr) {
                err("Failed to allocate memory for arguments.");
                return pr_fail();
            }
            memcpy(args_ptr, args.many.nodes, args.many.count*sizeof(Node*));
            fn_call->fn_call.args = args_ptr;
            fn_call->fn_call.args_count = args.many.count;
        }
        consume(pctx);
        return pr_ok(fn_call);
    } else if (current(pctx).type == TokenOpenSquare) {
		Token paren = consume(pctx);
		Node* expr  = parse_expression(pctx).node;
		if (!expr) {
			err("Failed to parse expression (array index).");
			return pr_fail();
		}
		if (current(pctx).type != TokenCloseSquare) {
			return expected_got("\"]\" after array index", current(pctx));
		}
		consume(pctx); // "]"
		Node* n = new_node(pctx, NodeIndex, paren);
		if (!n) {
			err("Failed to allocate new node.");
			return pr_fail();
		}
		n->index.term = primary;
		n->index.index_expression = expr;
		return pr_ok(n);
        // TODO index
    }
    return pr_ok(primary);
} // [expr] (params) ++ --
ParseRes parse_cast(ParserCtx* pctx) { // reimplement
    return parse_postfix(pctx);
} // (type) and what not
ParseRes parse_unary(ParserCtx* pctx) {
    Token op = current(pctx);
    if (    op.type == TokenStar
        ||  op.type == TokenAmpersand
        ||  op.type == TokenMinus
        ||  op.type == TokenBang
        ||  op.type == TokenTilde) {
        consume(pctx); // op "*" | "&" | "-" | "!" | "~"
        Node* target = parse_unary(pctx).node;
        if (!target) {
            err("Failed to parse unary expression.");
            return pr_fail();
        }
        Node* unary = new_node(pctx, NodeUnary, op);
        if (!unary) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        switch (op.type) {
            case TokenStar:         unary->unary.type = UnDeref; break;
            case TokenAmpersand:    unary->unary.type = UnRef; break;
            case TokenMinus:        unary->unary.type = UnNegative; break;
            case TokenBang:         unary->unary.type = UnNot; break;
            case TokenTilde:        unary->unary.type = UnCompliment; break;
            default: break; // can't happen
        }
        unary->unary.target = target;
        return pr_ok(unary);
    } else {
        Node* cast = parse_cast(pctx).node;
        if (!cast) {
            err("failed to parse cast expression.");
            return pr_fail();
        }
        return pr_ok(cast);
    }
} //  * & - ~ !
ParseRes parse_multiplicative(ParserCtx* pctx) {
    Node* unary = parse_unary(pctx).node;
    if (!unary) {
        err("Failed to parse unary expression.");
        return pr_fail();
    }
    while (current(pctx).type == TokenStar
        || current(pctx).type == TokenSlash
        || current(pctx).type == TokenPercent) {
        Token op = consume(pctx);  // "*" | "/" | "%"
        Node* rhs_unary = parse_unary(pctx).node;
        if (!rhs_unary) {
            err("Failed to parse rhs unary expression.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        switch (op.type) {
            case TokenStar:         n->binop.type = OpMlt; break;
            case TokenSlash:        n->binop.type = OpDiv; break;
            case TokenPercent:      n->binop.type = OpMod; break;
            default: break; // can't happen
        }
        n->binop.left = unary;
        n->binop.right = rhs_unary;
        unary = n;
    }
    return pr_ok(unary);
} // * / %
ParseRes parse_additive(ParserCtx* pctx) {
    Node* multiplicative = parse_multiplicative(pctx).node;
    if (!multiplicative) {
        err("Failed to parse multiplicative expression.");
        return pr_fail();
    }
    while (current(pctx).type == TokenPlus
        || current(pctx).type == TokenMinus) {
        Token op = consume(pctx);  // "+" | "-"
        Node* rhs_multiplicative = parse_multiplicative(pctx).node;
        if (!rhs_multiplicative) {
            err("Failed to parse rhs multiplicative expression.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = op.type == TokenPlus ? OpAdd : OpSub;
        n->binop.left = multiplicative;
        n->binop.right = rhs_multiplicative;
        multiplicative = n;
    }
    return pr_ok(multiplicative);
} // + -
ParseRes parse_bit_shift(ParserCtx* pctx) {
    Node* additive = parse_additive(pctx).node;
    if (!additive) {
        err("Failed to parse additive expression.");
        return pr_fail();
    }
    while (current(pctx).type == TokenShiftL
        || current(pctx).type == TokenShiftR) {
        Token op = consume(pctx);  // "<<" | ">>"
        Node* rhs_additive = parse_additive(pctx).node;
        if (!rhs_additive) {
            err("Failed to parse rhs additive expression.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = op.type == TokenShiftL ? OpLSh : OpRSh;
        n->binop.left = additive;
        n->binop.right = rhs_additive;
        additive = n;
    }
    return pr_ok(additive);
} // << >>
ParseRes parse_relational_comp(ParserCtx* pctx) {
    Node* bit_shift = parse_bit_shift(pctx).node;
    if (!bit_shift) {
        err("Failed to parse shift expression.");
        return pr_fail();
    }
    while (current(pctx).type == TokenLess
        || current(pctx).type == TokenGreater
        || current(pctx).type == TokenLessEqual
        || current(pctx).type == TokenGreaterEqual ) {
        Token op = consume(pctx);  // "<=" | ">=" | "<" | ">"
        Node* rhs_bit_shift = parse_bit_shift(pctx).node;
        if (!rhs_bit_shift) {
            err("Failed to parse rhs shift expression.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
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
    return pr_ok(bit_shift);
} // <= >= < >
ParseRes parse_logical_comp(ParserCtx* pctx) {
    Node* relational_comp = parse_relational_comp(pctx).node;
    if (!relational_comp) {
        err("Failed to parse relational comparasion.");
        return pr_fail();
    }
    while (current(pctx).type == TokenEqual
        || current(pctx).type == TokenNotEqual) {
        Token op = consume(pctx); // "==" | "!="
        Node* rhs_relational_comp = parse_relational_comp(pctx).node;
        if (!rhs_relational_comp) {
            err("Failed to parse rhs relational comp.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = op.type == TokenEqual ? OpEq : OpNeq;
        n->binop.left = relational_comp;
        n->binop.right = rhs_relational_comp;
        relational_comp = n;
    }

    return pr_ok(relational_comp);
}
ParseRes parse_bitwise_and(ParserCtx* pctx) {
    Node* logical_comp = parse_logical_comp(pctx).node;
    if (!logical_comp) {
        err("Failed to parse logical comparasion.");
        return pr_fail();
    }
    while (current(pctx).type == TokenAmpersand ) {
        Token op = consume(pctx); // "&" | "|"
        Node* rhs_locical_and = parse_logical_comp(pctx).node;
        if (!rhs_locical_and) {
            err("Failed to parse rhs logical comp.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = OpAnd;
        n->binop.left = logical_comp;
        n->binop.right = rhs_locical_and;
        logical_comp = n;
    }

    return pr_ok(logical_comp);
}
ParseRes parse_bitwise_xor(ParserCtx* pctx) {
    Node* bw_and = parse_bitwise_and(pctx).node;
    if (!bw_and) {
        err("Failed to parse bitwise and.");
        return pr_fail();
    }
    while (current(pctx).type == TokenCaret) {
        Token op = consume(pctx); // "^"
        Node* rhs_bw_and = parse_bitwise_and(pctx).node;
        if (!rhs_bw_and) {
            err("Failed to parse rhs bitwise and.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = OpXor;
        n->binop.left = bw_and;
        n->binop.right = rhs_bw_and;
        bw_and = n;
    }

    return pr_ok(bw_and);
}
ParseRes parse_bitwise_or(ParserCtx* pctx) {
    Node* bw_xor = parse_bitwise_xor(pctx).node;
    if (!bw_xor) {
        err("Failed to parse bitwise xor.");
        return pr_fail();
    }
    while (current(pctx).type == TokenPipe) {
        Token op = consume(pctx); // "|"
        Node* rhs_bw_xor = parse_bitwise_xor(pctx).node;
        if (!rhs_bw_xor) {
            err("Failed to parse rhs bitwise xor.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = OpOr;
        n->binop.left = bw_xor;
        n->binop.right = rhs_bw_xor;
        bw_xor = n;
    }

    return pr_ok(bw_xor);
}

ParseRes parse_logical_and(ParserCtx* pctx) {
    Node* bw_or = parse_bitwise_or(pctx).node;
    if (!bw_or) {
        err("Failed to parse bitwise or.");
        return pr_fail();
    }
    while (current(pctx).type == TokenAndAnd) {
        Token op = consume(pctx); // "&&"
        Node* rhs_bw_or = parse_bitwise_or(pctx).node;
        if (!rhs_bw_or) {
            err("Failed to parse rhs bitwise or.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = OpAndAnd;
        n->binop.left = bw_or;
        n->binop.right = rhs_bw_or;
        bw_or = n;
    }

    return pr_ok(bw_or);
}
ParseRes parse_logical_or(ParserCtx* pctx) {
    Node* logical_and = parse_logical_and(pctx).node;
    if (!logical_and) {
        err("Failed to parse logical and.");
        return pr_fail();
    }
    while (current(pctx).type == TokenOrOr) {
        Token op = consume(pctx); // "||"
        Node* rhs_locical_and = parse_logical_and(pctx).node;
        if (!rhs_locical_and) {
            err("Failed to parse rhs logical and.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, op);
        if (!n) {
            err("Failed to allocate memory for new node.");
            return pr_fail();
        }
        n->binop.type = OpOrOr;
        n->binop.left = logical_and;
        n->binop.right = rhs_locical_and;
        logical_and = n;
    }

    return pr_ok(logical_and);
}
ParseRes parse_conditional(ParserCtx* pctx) {
    Node* logical_or = parse_logical_or(pctx).node;
    if (!logical_or) {
        err("Failed to parse logical or.");
        return pr_fail();
    }
    // if its ... ? ... : ... ;
    if (current(pctx).type == TokenQuestion) {
        Token qm = consume(pctx); // "?"
        Node* expr = parse_expression(pctx).node;
        if(!expr) {
            err("Failed to parse expression.");
            return pr_fail();
        }
        if (current(pctx).type != TokenColon) {
            return expected_got("\":\" after a conditional"
                                "statement", consume(pctx));
        }
		consume(pctx); // ":"
        Node* next = parse_conditional(pctx).node;
        if (!next) {
            err("Failed to parse next conditional statement.");
            return pr_fail();
        }
        Node* conditional = new_node(pctx, NodeConditional, qm);
        if (!conditional) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        conditional->conditional.condition = logical_or;
        conditional->conditional.left_true = expr;
        conditional->conditional.right_false = next;
        logical_or = conditional;
    }
    return pr_ok(logical_or);
}
ParseRes parse_assignment(ParserCtx* pctx) {
    Node* lvalue = parse_conditional(pctx).node;
    if (!lvalue) {
        err("Failed to parse assignment.");
        return pr_fail();
    }
    
    if (current(pctx).type == TokenAssign) {
        if (!is_lvalue(lvalue)) {
            err("expression is not an lvalue.");
            return pr_fail();
        }
        Node* n = new_node(pctx, NodeBinOp, consume(pctx)); // ","
        if (!n) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        n->binop.type = OpAssign;
        
        Node* rhs_assignment = parse_assignment(pctx).node;
        if (!rhs_assignment) {
            err("Failed to parse rhs assignment.");
            return pr_fail();
        }
        n->binop.left = lvalue;
        n->binop.right = rhs_assignment;
        lvalue = n;
    }

    return pr_ok(lvalue);
}
ParseRes parse_expression(ParserCtx* pctx) {
    Node* assignment = parse_assignment(pctx).node;
    if (!assignment) {
        err("Failed to parse assignment.");
        return pr_fail();
    }
    
	// info("Current after assignemtn: %s", get_token_data(current(pctx)));
    while (current(pctx).type == TokenComma) {
		// info("binop??");
        Node* n = new_node(pctx, NodeBinOp, consume(pctx)); // ","
        if (!n) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        
        Node* rhs_assignment = parse_assignment(pctx).node;
        if (!rhs_assignment) {
            err("Failed to parse rhs assignment.");
            return pr_fail();
        }
        n->binop.left = assignment;
        n->binop.right = rhs_assignment;
        assignment = n;
    }

    return pr_ok(assignment);
}
ParseRes parse_expression_statement(ParserCtx* pctx) {
	Node* n = parse_expression(pctx).node;
	if (!n) {
		err("Failed to parse expression.");
		return pr_fail();
	}
	if (current(pctx).type != TokenSemicolon) {
		return expected_got("\";\" at the end of an expression statement",
				current(pctx));
	}
	consume(pctx); // ";"
	return pr_ok(n);
}
/*

lvalue          ::= IDENTIFIER
                 |  primary index
                 |  "*" postfix;

unary           ::= ("*" | "&" | "-" | "!") unary
                 |  postfix ;

postfix         ::= primary { fn_call | index | "." IDENTIFIER } ;

primary         ::= IDENTIFIER
                 |  NUMBER
                 |  "(" expression ")" ;

assignment_op   ::= ( "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" )
*/

// arrays still dont' work

ParseRes parse_let(ParserCtx* pctx) {
    consume(pctx); // let
    if (current(pctx).type != TokenIdent) {
        return expected_got("identifier after \"let\"", current(pctx));
    }
    // get name
    Name identifier = current(pctx).ident;
    // allocate var declaration node
    Node n;
    n.type = NodeVarDec;
    n.var_dec.name = identifier;
    n.var_dec.value = NULL;
    Node* var_dec_node = arena_add_node(pctx->ast->arena, n);
    consume(pctx); // identifier
    // parse type
    
    if (current(pctx).type == TokenColon) {
        consume(pctx);
        // parse type
        Node* type_node = parse_type(pctx).node;
        if (!type_node) {
            err("Failed to parse type.");
            return pr_fail();
        }
        if (type_node->type != NodeTypeData) {
            err("Exptected type node, but got something else"
                "(which is completely wrong)");
            return pr_fail();
        }
        var_dec_node->var_dec.type = &type_node->type_data;
    } else {
        return expected_got("\":\"", current(pctx));
    }


    // if it's a semicolon then just declare eg: let i;
    if (current(pctx).type == TokenSemicolon) {
        consume(pctx);
    } else if (current(pctx).type == TokenAssign) {
        consume(pctx); // "="
        // parse expression. block assignment can be everything
        Node* expr = parse_expression(pctx).node;
        if (!expr) {
            err("Failed to parse expression in var declaration.");
            return pr_fail();
        }
        if (current(pctx).type != TokenSemicolon) { 
            return expected_got("semicolon after expression", current(pctx));
        }
        consume(pctx);
        var_dec_node->var_dec.value = expr;
        // goes strainght to return
    } else {
        return expected_got("\";\", type or assignment after variable name",
                            current(pctx));
    }
    return pr_ok(var_dec_node);
}


ParseRes parse_fn(ParserCtx* pctx) {
    consume(pctx); // eat fn
    Token fn_name = consume(pctx);

    // fn dec
    Node fn_dec;
    fn_dec.fn_dec.args_count = 0;
    fn_dec.fn_dec.args = 0;

    if (fn_name.type != TokenIdent) {
        return expected_got("function name",current(pctx));
    }
    if (current(pctx).type != TokenOpenParen) {
        return expected_got("\"(\"", current(pctx));
    }
    consume(pctx); // "("
    // parse args
    if (current(pctx).type != TokenCloseParen) {
        size_t args_count = 0;
        Argument* args = arena_alloc(&pctx->gpa, 10*sizeof(Argument));
		if (!args) {
			err("Failed to allocate arg space.");
			return pr_fail();
		}
        for (;args_count < 10;args_count++) {
            if (current(pctx).type != TokenIdent)
                return expected_got("identifier for arg name", current(pctx));
            Token name = consume(pctx); // name
            if (current(pctx).type != TokenColon) {
                return expected_got("\":\" (type definition)", current(pctx));
            }
            consume(pctx); // ":"
            Node* type = parse_type(pctx).node; // type
            if (!type) {
                err("Failed to parse type in arg declaration.");
                return pr_fail();
            }
            Argument arg;
            arg.type = &type->type_data; // arena allocated node persists
            arg.name = name.ident;
            args[args_count++] = arg;
            // info("next: %s", get_token_data(current(pctx)));
            if (current(pctx).type == TokenComma) {
                consume(pctx);
                continue;
            }
            if (current(pctx).type == TokenCloseParen) break;
            else  return expected_got("\",\" or \")\"", current(pctx));
        }

        fn_dec.fn_dec.args = args;
        fn_dec.fn_dec.args_count = args_count;
    }
    consume(pctx); // ")"
    // parse return type
    if (current(pctx).type == TokenMinus && peek(pctx).type == TokenGreater) {
        consume(pctx); // "-"
        consume(pctx); // ">"
        Node* type_ptr = parse_type(pctx).node;
        if (!type_ptr) {
            err("Failed to parse fn return type.");
            return pr_fail();
        }
        if (type_ptr->type != NodeTypeData) {
            err("Exptected type node, got something else"
                "(shouldn't happend at all btw).");
            return pr_fail();
        }
        fn_dec.fn_dec.return_type = &type_ptr->type_data; // node persists
    } else {
        Node* type_ptr = new_node(pctx, NodeTypeData, (Token){0});
        if (!type_ptr) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        type_ptr->type_data.name = (Name){.name="void", .length=4};
        type_ptr->type_data.type = tt_to_determinate;
        type_ptr->type_data.ptr = 0;
        fn_dec.fn_dec.return_type = &type_ptr->type_data; // node persists
    }
    // expect for block
    if (current(pctx).type != TokenOpenBrace) {
        return expected_got("\"{\"", current(pctx));
    }
    // consume(pctx) "{" and "}" in parse block
    // parse block statement
    Node* fn_body = parse_block_statement(pctx).node;

    fn_dec.type = NodeFnDec;
    fn_dec.fn_dec.name = fn_name.ident;
    fn_dec.fn_dec.body = fn_body;
    return pr_ok(arena_add_node(pctx->ast->arena, fn_dec));
}

ParseRes parse_return(ParserCtx* pctx) {
	if (current(pctx).type != TokenKeyword ||
			current(pctx).kw != KwReturn) {
		return expected_got("\"return\" keyword", current(pctx));
	}
	Token ret = consume(pctx); // "return"
    // "return;"
    if (current(pctx).type == TokenSemicolon) {
        consume(pctx); // ";"
        Node* ret_node = new_node(pctx, NodeRet, ret);
        if (!ret_node) {
            err("Failed to allocate new node.");
            return pr_fail();
        }
        ret_node->ret = NULL;
        return pr_ok(ret_node);
    }

	Node* expr = parse_expression(pctx).node;
	if (!expr) {
		err("Failed to parse expression/return.");
		return pr_fail();
	}
	if (current(pctx).type != TokenSemicolon) {
		return expected_got("\";\" after return", current(pctx));
	}
	consume(pctx); // ";"
	Node* ret_node = new_node(pctx, NodeRet, ret);
	if (!ret_node) {
		err("Failed to allocate new node.");
		return pr_fail();
	}
	ret_node->ret = expr;
	return pr_ok(ret_node);
}

ParseRes parse_expression_return(ParserCtx* pctx) {
	if (current(pctx).type == TokenKeyword) {
		return parse_return(pctx); // catch invalid kw_type in here
	} else {
		Node* expr = parse_expression_statement(pctx).node;
		if (!expr) {
			err("Failed to parse expression statement.");
			return pr_fail();
		}
		return pr_ok(expr);
	}
}
ParseRes parse_if(ParserCtx* pctx) {

    Token _if = consume(pctx); // "if"

	Node* base_condition = parse_expression(pctx).node;

	if (!base_condition) {
		err("Failed to parse condition.");
		return pr_fail();
	}
	Node* n = new_node(pctx, NodeIfElse, _if);
	if (!n) {
		err("Failed to allocate new node.");
		return pr_fail();
	}

	// increase at next iteration
	// conditions[blocks_count] = base_condition;

	/*
	 * parse block:
	 * 	if a != b {
	 * 		let c = a + b;
	 * 		return c;
	 * 	}
	 */
	Node* block = NULL;
	if (current(pctx).type == TokenOpenBrace) {
		ParseRes pr = parse_block_statement(pctx);
		if (pr.ok == PrFail) {
			err("Failed to parse block statement");
			return pr_fail();
		}
		block = pr.node;
	} else {
		Node* expr = parse_expression_return(pctx).node;
		if (!expr) {
			err("Failed to parse expression statement");
			return pr_fail();
		}
		block = expr;
	}
	if (!block || !base_condition) {
		err("Failed to parse if statement.");
		return pr_fail();
	}



	size_t blocks_count = 0;
	Node** blocks = arena_alloc(&pctx->gpa, 10*sizeof(Node*));;
	Node** conditions = arena_alloc(&pctx->gpa, 10*sizeof(Node*));;
	/* while the next token is else keep parsing
	 * 	else if a == b {
	 * 		return a;
	 * 	}
	 */
    n->if_else_con.else_block = 0;
	while ( current(pctx).type == TokenKeyword
		&& current(pctx).kw == KwElse) {
		Token else_token = consume(pctx); // else
		
		if (current(pctx).type == TokenKeyword // else if
				&& current(pctx).kw == KwIf) { // has a condition
			Token _next_if = consume(pctx); // "if"
			Node* alternate_condition = parse_expression(pctx).node;
			if (!alternate_condition) {
				err("Failed to parse condition.");
				return pr_fail();
			}

			// parse block
			Node* alternate_block = NULL;
			if (current(pctx).type == TokenOpenBrace) {
				ParseRes pr = parse_block_statement(pctx);
				if (pr.ok == PrFail || !pr.node) {
					err("Failed to parse block statement");
					return pr_fail();
				}
				alternate_block = pr.node;
			} else {
				Node* expr = parse_expression_statement(pctx).node;
				if (expr) {
					err("Failed to parse expression statement");
					return pr_fail();
				}
				alternate_block = expr;
			}
			if (!alternate_block || !alternate_condition) {
				err("Failed to parse if statement.");
				return pr_fail();

			}
			conditions[blocks_count] = alternate_condition;
			blocks[blocks_count] = alternate_block;
			blocks_count++;

		} else { // parse else (we have "else" already
			Node* else_block = 0;
			if (current(pctx).type == TokenOpenBrace) {
				ParseRes pr = parse_block_statement(pctx);
				if (pr.ok == PrFail || !pr.node) {
					err("Failed to parse block statement");
					return pr_fail();
				}
				else_block = pr.node;
			} else {
				Node* expr = parse_expression_return(pctx).node;
				if (!expr) {
					err("Failed to parse expression return");
					return pr_fail();
				}
				else_block = expr;
			}
			if (!else_block) {
				err("Failed to parse else block");
				return pr_fail();
			}
			n->if_else_con.else_block = else_block;
		}

	}
    n->if_else_con.base_condition = base_condition;
    n->if_else_con.base_block = block;

	if (blocks_count > 0) {
		n->if_else_con.count = blocks_count;
		n->if_else_con.alternate_conditions = conditions;
		n->if_else_con.alternate_blocks = blocks;
	} else {
		n->if_else_con.count = 0;
		n->if_else_con.alternate_conditions = 0;
		n->if_else_con.alternate_blocks = 0;
	}

    return pr_ok(n);
}
ParseRes parse_statement(ParserCtx* pctx) {
    if (current(pctx).type == TokenKeyword) {
        if (current(pctx).kw == KwLet) {
            ParseRes pr = parse_let(pctx);

            return pr;
        } else if (current(pctx).kw == KwFn) {
            return parse_fn(pctx);
        } else if (current(pctx).kw == KwReturn) {
            consume(pctx); 
            // just expression
            Node* expr =  parse_expression(pctx).node;
            if (!expr) {
                err("Failed to parse expression.");
                return pr_fail();
            }
            if(current(pctx).type != TokenSemicolon) {
                return expected_got("\";\"", current(pctx));
            }
            consume(pctx); // ";"
            Node n;
            n.type = NodeRet;
            n.ret = expr;
            return pr_ok(arena_add_node(pctx->ast->arena,n));
        } else if (current(pctx).kw == KwIf) {
            return parse_if(pctx);;
        } else if (current(pctx).kw == KwStruct) {
            TODO("implement struct");
        } else {
            err("Invalid keyword: %s.\n", get_keyword_name(current(pctx).kw));
            return pr_fail();
        }
    } else {
        Node* expr = parse_expression(pctx).node;
        if (!expr) {
            err("Failed to parse expression/statement.");
            return pr_fail();
        }
        if (current(pctx).type != TokenSemicolon) {
            return expected_got("\";\"", current(pctx));
        }
        consume(pctx);
        // err("unexpected %s.", get_token_type(current(pctx).type));
        return pr_ok(expr);
    }

    return  expected_got("a valid statement", current(pctx)); // TODO make this make sense
    consume(pctx); // ivalid token
    return pr_fail();
}
ParseRes parse_block_statement(ParserCtx* pctx) {
    if (current(pctx).type != TokenOpenBrace) {
        return expected_got("\"{\"", current(pctx));
    }
    consume(pctx); // "{"
    
    Node block;
    block.type = NodeBlock;
    Node** block_statements = arena_alloc(pctx->ast->arena,
            100*sizeof(Node*)); // 100 nodes
    size_t block_index = 0;
    while (current(pctx).type != TokenCloseBrace) {
        if (current(pctx).type == TokenString // print statement
            && peek(pctx).type == TokenSemicolon) {
            Node* n = new_node(pctx, NodePrintString, consume(pctx));// string
            consume(pctx); // ";" 
            if (!n) {
                err("Failed to allocate new node.");
                return pr_fail();
            }
            n->print_string.string = n->token.string;
            block_statements[block_index++] = n;
        }
        // parse_statement
        ParseRes pr = parse_statement(pctx);
        if (pr.ok == PrOk) {
            block_statements[block_index++] = pr.node;
        } else if (pr.ok == PrMany) {
            // hard coded
            for (size_t i = 0; i < pr.many.count && i < 100; i++)
                block_statements[block_index++] = pr.many.nodes[i];
        } else {
            err("Failed to parse statement.");
            return pr_fail();
        }
    }
    if (block_index >= 100) {
        warn("more nodes that space in block: %d (limit 100)", block_index);
    }
    consume(pctx); // "}"
    block.block.nodes = block_statements;
    block.block.nodes_count = block_index;
    ParseRes pr = pr_ok(arena_add_node(pctx->ast->arena, block));
    return pr;
}
// return 1 on succecss
ParseRes parse_top_level_statement(ParserCtx* pctx) {
    if (current(pctx).type == TokenSemicolon) {
        consume(pctx);
        return pr_ok(NULL); // ok ig?
    }
    // only kw for now
    if (current(pctx).type != TokenKeyword) {
        return expected_got("keyword", current(pctx));
    }
    // we know it's a keyword
    // let <name> ...
    if (current(pctx).kw == KwLet) {
        Node* expr = parse_let(pctx).node;
        if (!is_cmpt_constant(expr)) {
            err("top level let must be a compile time constant");
            return pr_fail();
        }
        return pr_ok(expr);
    } else if (current(pctx).kw == KwFn) {
        return parse_fn(pctx);
    } else {
        // err("Invalid keyword at %zu:%zu", current(pctx).line, current(pctx).col);
        err("Invalid keyword at %zu:%zu", current(pctx).line, current(pctx).col);
        return pr_fail();
    }
}

