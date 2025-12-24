#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include "parse_number.c"
#include <assert.h>
#include <stdatomic.h>
#include <stddef.h>
#include <stdio.h>

ParseRes expected_got_1(char* expected, Token got) {
    err("Exptected %s, got: %s.", expected, get_token_type(got.type));
    return pr_fail();
}
#define expected_got(expected, got) err("%d Exptected " expected ", got: %s.", __LINE__, get_token_type(got.type)), pr_fail();

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
#define current  tokens[*i]
#define peek tokens[(*i) + 1]
#define consume tokens[(*i)++]


ParseRes parse_args(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node* nodes[10];
    size_t count = 0;

    do {
        if (current.type == TokenComma && count > 0) consume;
        else if (current.type == TokenComma && count == 0)
            return err("cant have empty args."), pr_fail();

        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (!expr) {
            err("failed to parse argument");
            continue; // try to parse next one
        }
        nodes[count++] = expr;

    } while(current.type == TokenComma); // ";"

    if (count > 10 ) {
        err("More than 10 arguments found. max is 10 for now (hard coded)");
        count = 10;
    }

    return pr_ok_many(nodes, count);
}

// one or more like: f()[0]; or a[2]();
// term, unary postix
ParseRes parse_primary(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type == TokenOpenParen) {
        consume; // "("
        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (!expr) {
            err("Failed to parse in term expression.");
            return pr_fail();
        }
        if (current.type != TokenCloseParen) {
            return expected_got("\")\"", current);
        } else {
            consume;
            return pr_ok(expr);
        }
    } else if (current.type == TokenNumber) {
        double out;
        if (!parse_number(current.number.name, current.number.length, &out)) {
            err("Failed to parse number.\n");
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
        Node n;
        n.type = NodeVar;
        n.var.name = ident;
        return pr_ok(arena_add_node(ast->arena, n));
    } else {
        return expected_got("primary (number or identifier)", current);
    }
    return pr_fail();
}
ParseRes parse_postfixes(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node* term = parse_primary(ast, tokens, i, len).node;
    if (!term) {
        err("Failed to parse term.");
        return pr_fail();
    }
    while (current.type == TokenOpenParen
        || current.type == TokenOpenSquare ) {
        if (current.type == TokenOpenParen) { // fn call
            consume; // "("
            Node n;
            n.type = NodeFnCall;
            n.fn_call.fn = term;
            n.fn_call.args = 0;
            n.fn_call.args_count = 0;
            if (current.type != TokenCloseParen) {
                ParseRes pr = parse_args(ast, tokens, i, len);
                if (pr.ok == PrFail) {
                    err("Failed to parse args");
                    return pr_fail();
                }
                n.fn_call.args = arena_alloc(ast->arena,
                                             sizeof(Node*) * pr.many.count);
                if (!n.fn_call.args) {
                    err("Failed to allocate memory for args using arena");
                    return pr_fail();
                }
                memcpy(n.fn_call.args, pr.many.nodes,
                       sizeof(Node*) * pr.many.count);
                n.fn_call.args_count = pr.many.count;
            } else if (current.type != TokenCloseParen) {
                return expected_got("\"(\"", current);
            }
            consume; // ")"
            term = arena_add_node(ast->arena,n);
        }
    }
    return pr_ok(term);
}

ParseRes parse_unary(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node* term;
    if (    current.type == TokenStar
         || current.type == TokenAmpersand
         || current.type == TokenMinus
         || current.type == TokenTilde
         || current.type == TokenBang
    ) {
        Token op = consume; // unary op
        term = parse_unary(ast, tokens, i, len).node;
        Node n;
        n.type = NodeUnary;
        switch (op.type) {
            case TokenAmpersand: { // &term
                n.unary.type = UnRef;
            } break;
            case TokenStar: { // *term
                n.unary.type = UnDeref;
            } break;
            case TokenMinus: { // -term
                n.unary.type = UnNegative;
            } break;
            case TokenBang: {
                n.unary.type = UnNot;
            } break;
            case TokenTilde: { // -term
                n.unary.type = UnCompliment;
            } break;
            default:
                err("Unhandled unary op type: %d.", op.type);
                return pr_fail();
        }
        n.unary.target = term;
        term = arena_add_node(ast->arena, n);
    } else {
        term = parse_postfixes(ast, tokens, i, len).node;
    }
    if (!term) {
        err("Failed to parse term.");
        return pr_fail();
    }


    return pr_ok(term);
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
    Node* lhs = parse_unary(ast, tokens, i, len).node;
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

        // aslways left associativity?
        // right association would be next min prec = min prec
        // use current prec since if min prec is 1 and prec of current op
        // is 5 (for examoke), min prec + 1 would be 2, so some ops less than 5
        // might be parsed first
        // assignment must be right associative
        Node* rhs;
        // right associativity: a = b = c -> a = (b=c)
        if (current_prec == get_precedence(OpAssign)) 
            rhs = prec_climbing(ast, tokens, i, len, current_prec).node;
        else // next one for left associativity: a = b = c -> (a=b) = c
            rhs = prec_climbing(ast, tokens, i, len, current_prec + 1).node;
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
ParseRes parse_let_2(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume; // let
    if (current.type != TokenIdent) {
        return expected_got("identifier", current);
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
            print_node(expr, 10);
            return pr_fail();
        }
        if (current.type != TokenSemicolon) { 
            return expected_got(";", current);
        }
        consume;
        var_dec_node->var_dec.value = expr;
        // goes strainght to return
    // add type here
    } else if (current.type == TokenColon) {
    } else {
        return expected_got(";\", type and/or expression", current);
    }
    return pr_ok(var_dec_node);
}
ParseRes parse_let(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume; // let
    if (current.type != TokenIdent) {
        return expected_got("identifier after \"let\"", current);
    }
    // get name
    Name identifier = current.ident;
    // allocate var declaration node
    Node n;
    n.type = NodeVarDec;
    n.var_dec.name = identifier;
    n.var_dec.value = NULL;
    Node* var_dec_node = arena_add_node(ast->arena, n);
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
            return expected_got("semicolon after expression", current);
        }
        consume;
        var_dec_node->var_dec.value = expr;
        // goes strainght to return
    // add type here
    } else if (current.type == TokenColon) {
    } else {
        return expected_got("\";\", type or assignment after variable name",
                            current);
    }
    return pr_ok(var_dec_node);
}


ParseRes parse_fn(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume; // eat fn
    Token fn_name = consume;
    if (fn_name.type != TokenIdent) {
        return expected_got("function name",current);
    }
    if (current.type != TokenOpenParen) {
        return expected_got("\"(\"", current);
    }
    consume; // "("
    // parse args
    if (current.type != TokenCloseParen) {
        return expected_got("\")\"", current);
    }
    consume; // ")"
    // parse return type
    //
    // expect for block
    if (current.type != TokenOpenBrace) {
        return expected_got("\"{\"", current);
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

ParseRes parse_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type == TokenKeyword) {
        if (current.kw == KwLet) {
            return parse_let(ast, tokens, i, len);
        } else if (current.kw == KwFn) {
            return parse_fn(ast, tokens, i, len);
        } else if (current.kw == KwReturn) {
            consume; 
            // just expression
            Node* expr =  parse_expression(ast, tokens, i, len).node;
            if (!expr) {
                err("Failed to parse expression.");
                return pr_fail();
            }
            if(current.type != TokenSemicolon) {
                return expected_got("\";\"", current);
            }
            consume; // ";"
            Node n;
            n.type = NodeRet;
            n.ret = expr;
            return pr_ok(arena_add_node(ast->arena,n));
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
            return expected_got("\";\"", current);
        }
        consume;
        // err("unexpected %s.", get_token_type(current.type));
        return pr_ok(expr);
    }

    /* return */  expected_got("a valid statement", current); // TODO make this make sense
    consume; // ivalid
    return pr_fail();
}
ParseRes parse_block_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type != TokenOpenBrace) {
        return expected_got("\"{\"", current);
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
            for (size_t i = 0; i < pr.many.count && i < 100; i++) // hard coded
                block_statements[block_index++] = pr.many.nodes[i];
        } else {
            err("Failed to parse statement.");
            return pr_fail();
        }
    }
    if (block_index >= 100) {
        warn("more nodes that space in block: %d (limit 100)", block_index);
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
        return expected_got("keyword", current);
    }
    // we know it's a keyword
    // let <name> ...
    if (current.kw == KwLet) {
        Node* expr = parse_let(ast, tokens, i, len).node;
        if (!is_cmpt_constant(expr)) {
            err("top level let must be a compile time constant");
            print_node(expr, 10);
            return pr_fail();
        }
        return pr_ok(expr);
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
