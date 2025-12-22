#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include "parse_number.c"
#include <assert.h>
#include <stddef.h>
#include <stdio.h>


int is_lvalue(Node* node) {
    return
    node->type == NodeUnary || // idk fix later maybe idk
    node->type == NodeVar || 
    node->type == NodeField || 
    node->type == NodeIndex ;
}
ParseRes parse_expression(AST* ast, Token* tokens, size_t* i, size_t len);
ParseRes parse_top_level_statement(AST* ast, Token* tokens, size_t* i, size_t len);
ParseRes parse_block_statement(AST* ast, Token* tokens, size_t* i, size_t len);

AST* parse(Lexer* l, Arena* a) {
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->max_nodes=1024;
    ast->nodes = (Node**)malloc(sizeof(Node*)*ast->max_nodes);
    ast->nodes_count = 0;
    ast->arena = a;

    Token* tokens = l->tokens;
    size_t len = l->tokens_count;

    #define current tokens[i]
    #define peek tokens[i+1]
    #define consume tokens[i++]
    size_t i = 0;
    while (i < len && current.type != TokenEOF) {
        ParseRes pr = parse_top_level_statement(ast, tokens, &i, len);
        if (pr.ok == PrFail) {
            err("Failed to parse top level statement.");
            free(ast->nodes);
            free(ast);
            return NULL;
        }
        if (pr.ok == PrOk) 
            ast_add_node(ast, pr.node);
        else if (pr.ok == PrMany) {
            // info("Many of size: %zu", pr.many.count);
            for (size_t i = 0; i < pr.many.count; i++) {
                ast_add_node(ast, pr.many.nodes[i]);
            }
        }
    }

    #undef consume
    #undef peek
    #undef current
    print_ast(ast);
    return ast;
}

Node* new_node(Node n) {
    return arena_add_node(NULL, n);
}
#define current tokens[*i]
#define peek tokens[(*i) + 1]
#define consume tokens[(*i)++]
// term, () and ! ~ -
ParseRes parse_term(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type == TokenOpenParen) {
        consume; // "("
        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (!expr) {
            err("Failed to parse in term expression.");
            return pr_fail();
        }
        if (current.type != TokenCloseParen) {
            err("Expected \")\", got: %s.", get_token_type(current.type));
            return pr_fail();
        } else {
            consume;
            return pr_ok(expr);
        }
    } else if (current.type == TokenNumber) {
        double out;
        if (!parse_number(current.number.name, current.number.length, &out)) {
            err( "Failed to parse number.\n");
            return pr_fail();
        }

        Node node;
        node.type = NodeNumLit;
        node.number.number = out;
        node.number.str_repr = current.number;
        consume; // number
        return pr_ok(arena_add_node(ast->arena,node));
    } else if (current.type == TokenIdent) {
        Name ident = consume.ident;
        // function call
        if (current.type == TokenOpenParen) {
            TODO("implement function call");
        // array access
        } else if (current.type == TokenOpenSquare) {
            TODO("implement array access");
        } else {
            Node n;
            n.type = NodeVar;
            n.var.name = ident;
            return pr_ok(arena_add_node(ast->arena, n));
        }
    // reference / dereference
    } else if (current.type == TokenAmpersand
                || current.type == TokenStar) {
        consume;
        // reference must be a term like var or (...)
        ParseRes pr = parse_term(ast, tokens, i, len);
        if (pr.ok != PrOk) {
            err("Faied to parse term for dereference.");
            return pr_fail();
        }
        Node n;
        n.type = NodeUnary;
        n.unary.type = TokenAmpersand ? UnRef : UnDeref;
        n.unary.target = pr.node;
        return pr_ok(arena_add_node(ast->arena, n));
    } else if (0) {

    } else {
        err("Expected term, got: %s.\n", get_token_data(current));

        return pr_fail();
    }
    return pr_fail();
}

int get_precedence(OpType op) {
    switch (op) {
        case OpAssign:
            return 1;

        case OpOrOr:
            return 2;

        case OpAndAnd:
            return 3;

        case OpOr:
            return 4;

        case OpXor:
            return 5;

        case OpAnd:
            return 6;

        case OpEq:
        case OpNeq:
            return 7;

        case OpLt:
        case OpGt:
        case OpLe:
        case OpGe:
            return 8;

        case OpLSh:
        case OpRSh:
            return 9;

        case OpAdd:
        case OpSub:
            return 10;

        case OpMlt:
        case OpDiv:
        case OpMod:
            return 11;

        default:
            return 0; // not a binary operator
    }
}
// (1 + 2) * 3 | 4;
// first multiply then or
// ((1+2)*3) | 4

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

ParseRes prec_climbing(AST* ast, Token* tokens, size_t* i,
                          size_t len, int min_prec) {
    if (min_prec <= 0) min_prec = 1;
    Node* lhs = parse_term(ast, tokens, i, len).node;
    if (!lhs) {
        err("Failed to parse term.\n");
        return pr_fail();
    }

    if (current.type == TokenCloseParen) {
        return pr_ok(lhs);
    }

    while (get_precedence(get_op(current)) >= min_prec) {
        if (current.type == TokenSemicolon) {
            err("For some fuckas reasong semicolon triggered a reparsing");
            assert(0);
        }
        OpType op = get_op(consume);
        int current_prec = get_precedence(op);

        // aslways left associating?
        // right association would be next min prec = min prec
        // use current prec since if min prec is 1 and prec of current op
        // is 5 (for examoke), min prec + 1 would be 2, so some ops less than 5
        // might be parsed first
        Node* rhs = prec_climbing(ast, tokens, i, len, current_prec+1).node;
        if (!rhs) {
            err("Failed to parse expression in expression");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = op;
        n.binop.left = lhs;
        n.binop.right = rhs;
        lhs = arena_add_node(ast->arena, n);
    }

    return pr_ok(lhs);
}

ParseRes parse_expression(AST* ast, Token* tokens, size_t* i, size_t len) {
    return prec_climbing(ast, tokens, i, len, 1); // 0 is invalid
}
ParseRes parse_top_level_let(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume; // let
    if (current.type != TokenIdent) {
        err("Expected identifier after \"let\", got: %s.\n",
            get_token_type(current.type));
        return pr_fail();
    }
    // get name
    Name identifier = current.ident;
    // allocate var declaration node
    Node n;
    n.type = NodeVarDec;
    n.var_dec.name = identifier;
    n.var_dec.value = NULL;
    Node* var_dec_node = arena_add_node(ast->arena, n);
    fflush(stdout);

    consume; // identifier

    // if it's a semicolon then just declare eg: let i;
    if (current.type == TokenSemicolon) {
        consume;
    } else if (current.type == TokenAssign) {
        consume; // "="
        // parse expression. block assignment can be everything
        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (!expr) {
            err("Failed to parse expression in var declaration.");
            return pr_fail();
        }
        // top level let can only be compile time constants
        if (!is_cmpt_constant(expr)) {
            err("top level let must be a compile time constant");
            return pr_fail();
        }
        if (current.type != TokenSemicolon) { 
            err("expected \";\" after expression, got: %s.",
                get_token_type(current.type));
            return pr_fail();
        }
        consume;
        var_dec_node->var_dec.value = expr;
        // goes strainght to return
    // add type here
    } else if (current.type == TokenColon) {
    } else {
        err("Expected \";\", type or assignment after variable name,"
            "but got: %s.", get_token_type(current.type));
        return pr_fail();
    }
    return pr_ok(var_dec_node);
}
ParseRes parse_let(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume; // let
    if (current.type != TokenIdent) {
        err("Expected identifier after \"let\", got: %s.\n",
            get_token_type(current.type));
        return pr_fail();
    }
    // get name
    Name identifier = current.ident;
    // allocate var declaration node
    Node n;
    n.type = NodeVarDec;
    n.var_dec.name = identifier;
    n.var_dec.value = NULL;
    Node* var_dec_node = arena_add_node(ast->arena, n);
    fflush(stdout);

    consume; // identifier

    // if it's a semicolon then just declare eg: let i;
    if (current.type == TokenSemicolon) {
        consume;
    } else if (current.type == TokenAssign) {
        consume; // "="
        // parse expression. block assignment can be everything
        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (!expr) {
            err("Failed to parse expression in var declaration.");
            return pr_fail();
        }
        if (current.type != TokenSemicolon) { 
            err("expected \";\" after expression, got: %s.",
                get_token_type(current.type));
            return pr_fail();
        }
        consume;
        var_dec_node->var_dec.value = expr;
        // goes strainght to return
    // add type here
    } else if (current.type == TokenColon) {
    } else {
        err("Expected \";\", type or assignment after variable name,"
            "but got: %s.", get_token_type(current.type));
        return pr_fail();
    }
    return pr_ok(var_dec_node);
}


ParseRes parse_fn(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume; // eat fn
    Token fn_name = consume;
    if (fn_name.type != TokenIdent) {
        err("Expected name, got: %s", get_token_type(fn_name.type));
        return pr_fail();
    }
    if (current.type != TokenOpenParen) {
        err("Expected \"(\", got: %s", get_token_type(current.type));
        return pr_fail();
    }
    consume; // "("
    // parse args
    if (current.type != TokenCloseParen) {
        err("Expected \")\", got: %s", get_token_type(current.type));
        return pr_fail();
    }
    consume; // ")"
    // parse return type
    //
    // expect for block
    if (current.type != TokenOpenBrace) {
        err("Expected \"{\", got: %s", get_token_type(current.type));
        return pr_fail();
    }
    // consume "{" and "}" in parse block
    // parse block statement
    Node* fn_body = parse_block_statement(ast, tokens, i, len).node;

    Node fn_dec;
    fn_dec.type = NodeFnDec;
    fn_dec.fn_dec.name = fn_name.ident;
    fn_dec.fn_dec.body = fn_body;
    return pr_ok(arena_add_node(ast->arena, fn_dec));
}

ParseRes parse_fn_call(AST* ast, Token* tokens, size_t* i, size_t len) {


    return pr_fail();
}

/* ParseRes parse_assignmet_expression(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node* expr = parse_expression(ast, tokens, i, len, 0).node;
    if (!expr) {
        consume; // invalid token
        err("Failed to parse normal expression");
        return pr_fail();
    }
    if (current.type == TokenAssign) {
        consume; // "="
        if (!is_lvalue(expr)) {
            err("Invalid expression. for an assignment, left value must be a valid lvalue. (var, deref, index, etc...)");
            print_node(expr, 0);
            return pr_fail();
        }
        Node* right = parse_assignmet_expression(ast, tokens, i, len).node;
        if (!right) {
            err("Failed to parse assignment expression");
            return pr_fail();
        }
        Node n;
        n.type = NodeAssignment;
        n.assignment.target = expr;
        n.assignment.value = right;
        return pr_ok(arena_add_node(ast->arena, n));
    }
    return pr_ok(expr);
}*/
ParseRes parse_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type == TokenKeyword) {
        if (current.kw == KwLet) {
            return parse_let(ast, tokens, i, len);
        } else if (current.kw == KwFn) {
            return parse_fn(ast, tokens, i, len);
        } else if (current.kw == KwIf) {
            TODO("implement if");
        } else if (current.kw == KwStruct) {
            TODO("implement struct");
        } else {
            err("Invalid keyword: %s.\n", get_keyword_name(current.kw));
            return pr_fail();
        }
    } else {
        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (!expr) {
            err("Failed to parse expression/statement.");
            return pr_fail();
        }
        if (current.type != TokenSemicolon) {
            err("expected semicolon after expression.");
            return pr_fail();
        }
        consume;
        // err("unexpected %s.", get_token_type(current.type));
        return pr_ok(expr);
    }
    err("expect something valid, got: %s", get_token_type(current.type));
    consume; // ivalid
    return pr_fail();
}
ParseRes parse_block_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type != TokenOpenBrace) {
        err("Expected \"{\" for block statement, got: %s",
            get_token_type(current.type));
        return pr_fail();
    }
    consume; // "{"
    
    Node block;
    block.type = NodeBlock;
    Node** block_statements = arena_alloc(ast->arena, 100*sizeof(Node*));
    size_t block_index = 0;
    while (current.type != TokenCloseBrace) {
        // parse_statement
        ParseRes pr = parse_statement(ast, tokens, i,  len);
        if (pr.ok == PrOk) {
            block_statements[block_index++] = pr.node;
        } else if (pr.ok == PrMany) {
            for (size_t i = 0; i < pr.many.count; i++)
                block_statements[block_index++] = pr.many.nodes[i];
        } else {
            err("Failed to parse statement.");
        }
    }
    consume; // "{"
    block.block.nodes = block_statements;
    block.block.nodes_count = block_index;
    ParseRes pr = pr_ok(arena_add_node(ast->arena, block));
    return pr;
}
// return 1 on succecss
ParseRes parse_top_level_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type == TokenSemicolon) {
        consume;
        return pr_ok(NULL); // ok ig?
    }
    // only kw for now
    if (current.type != TokenKeyword) {
        err( "Expected keyword, got: %s.\n", get_token_type(current.type));
        err("%s", get_token_data(current));
        return pr_fail();
    }
    // we know it's a keyword
    // let <name> ...
    if (current.kw == KwLet) {
        return parse_top_level_let(ast, tokens, i,  len);
    } else if (current.kw == KwFn) {
        return parse_fn(ast, tokens, i, len);
    } else {
        // err("Invalid keyword at %zu:%zu", current.line, current.col);
        err("Invalid keyword at %zu:%zu", current.line, current.col);
        return pr_fail();
    }
}

#undef consume
#undef peek
#undef current
