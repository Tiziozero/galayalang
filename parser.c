#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include "parse_number.c"
#include "parser_get_type.c"
#include <assert.h>
#include <stdatomic.h>
#include <stddef.h>
#include <stdio.h>
// #include <time.h>

void _err_sym_exists(Name name) {
    err("symbol \"%.*s\" already exists.",
        (int)name.length,name.name);
}
// returns 1 on succeess
int ss_create_symbols(SymbolStore* ss, Node** nodes, size_t len) {
    for (size_t i = 0; i < len; i++) {
        Node* node = nodes[i];
        switch (node->type) {
            case NodeVarDec: {
                Variable v;
                v.name = node->var_dec.name;
                v.type = node->var_dec.type;
                if (!ss_new_var(ss,v)) {
                    err("Failed to create symbol var: \"%.*s\".",
                        (int)v.name.length,v.name.name);
                    if (ss_sym_exists(ss, v.name)) {
                        _err_sym_exists(v.name);
                    } else {
                        err("no clue why.");
                    }
                    break;
                }
                info("New var: \"%.*s\".",
                        (int)v.name.length,v.name.name);
            } break;
            case NodeFnDec: {
                // see if it exists first, no need to do aldat later
                Name name = node->fn_dec.name;
                if (ss_sym_exists(ss, name)) {
                    _err_sym_exists(name);
                    return 0;
                }
                SymbolStore* _ss = malloc(sizeof(SymbolStore));
                if (!ss) {
                    err("Failed to allocate symbol store.");
                    return 0;
                }
                _ss->syms_capacity = 256;
                _ss->syms = malloc(ss->syms_capacity*sizeof(Symbol));
                if (!ss->syms) {
                    err("Failed to allocate memory for symbol store symbols.");
                    return 0;
                }
                _ss->syms_count = 0;

                node->fn_dec.body->block.ss = _ss;
                if (!ss_create_symbols(_ss, node->fn_dec.body->block.nodes,
                                     node->fn_dec.body->block.nodes_count)) {
                    err("Failed to create symbols for fnnction body.");
                    return 0;
                }
                Function fn;
                fn.name = node->fn_dec.name;
                fn.return_type = node->fn_dec.return_type;
                ss_new_fn(_ss, fn);
            } break;
            case NodeBinOp: {
            } break;
            // TODO: finish
            default: break;
        }
    }
    return 1;
}

ParseRes expected_got_1(char* expected, Token got) {
    err("Exptected %s, got: %s.", expected, get_token_type(got.type));
    return pr_fail();
}
#define expected_got(expected, got) err("%d Exptected " expected \
        ", got: %s.", __LINE__, get_token_type(got.type)), pr_fail();

int is_lvalue(Node* node) {
    return
    node->type == NodeUnary || // idk fix later maybe idk
    node->type == NodeVar || 
    node->type == NodeField || 
    node->type == NodeIndex ;
}
ParseRes parse_expression(ParserCtx* pctx);
ParseRes parse_top_level_statement(ParserCtx* pctx);
ParseRes parse_block_statement(ParserCtx* pctx);

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
            // info("Many of size: %zu", pr.many.count);
            for (size_t i = 0; i < pr.many.count; i++) {
                ast_add_node(pctx->ast, pr.many.nodes[i]);
            }
        }
    }

    // symbols etc
    ss_create_symbols(&pctx->symbols, pctx->ast->nodes,
                        pctx->ast->nodes_count);
    return pctx;
    print_ast(pctx->ast);

    for (size_t i = 0; i < pctx->ast->nodes_count; i++) {
        Node* expr = pctx->ast->nodes[i];
        type_t t;
        int res = get_expression_type(pctx, expr, &t);
        if (res != 0) {
            err("failed to typecheck expression: %d.", res);
        }
    }

    return pctx;
}

Node* new_node(Node n) {
    return arena_add_node(NULL, n);
}


ParseRes parse_args(ParserCtx* pctx) {
    Node* nodes[10];
    size_t count = 0;

    do {
        if (current(pctx).type == TokenComma && count > 0) consume(pctx);
        else if (current(pctx).type == TokenComma && count == 0)
            return err("cant have empty args."), pr_fail();

        Node* expr = parse_expression(pctx).node;
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

    return pr_ok_many(nodes, count);
}

ParseRes parse_primary(ParserCtx* pctx) {
    if (current(pctx).type == TokenOpenParen) {
        consume(pctx); // "("
        Node* expr = parse_expression(pctx).node;
        if (!expr) {
            err("Failed to parse in term expression.");
            return pr_fail();
        }
        if (current(pctx).type != TokenCloseParen) {
            return expected_got("\")\"", current(pctx));
        } else {
            consume(pctx);
            return pr_ok(expr);
        }
    } else if (current(pctx).type == TokenNumber) {
        // number so set type to number
        double out;
        if (!parse_number(current(pctx).number.name, current(pctx).number.length, &out)) {
            err("Failed to parse number.\n");
            return pr_fail();
        }
        Node node;
        node.type = NodeNumLit;
        node.number.number = out;
        node.number.str_repr = current(pctx).number;
        // node.type_type.type = u64_t;
        consume(pctx); // number
        return pr_ok(arena_add_node(pctx->ast->arena,node));
    } else if (current(pctx).type == TokenIdent) {
        Name ident = consume(pctx).ident;
        Node n;
        n.type = NodeVar;
        n.var.name = ident;
        return pr_ok(arena_add_node(pctx->ast->arena, n));
    } else {
        return expected_got("primary (number or identifier)", current(pctx));
    }
    return pr_fail();
}
ParseRes parse_postfixes(ParserCtx* pctx) {
    Node* term = parse_primary(pctx).node;
    if (!term) {
        err("Failed to parse term.");
        return pr_fail();
    }
    while (current(pctx).type == TokenOpenParen
        || current(pctx).type == TokenOpenSquare ) {
        if (current(pctx).type == TokenOpenParen) { // fn call
            consume(pctx); // "("
            Node n;
            n.type = NodeFnCall;
            n.fn_call.fn = term;
            n.fn_call.args = 0;
            n.fn_call.args_count = 0;
            if (current(pctx).type != TokenCloseParen) {
                ParseRes pr = parse_args(pctx);
                if (pr.ok == PrFail) {
                    err("Failed to parse args");
                    return pr_fail();
                }
                n.fn_call.args = arena_alloc(pctx->ast->arena,
                                             sizeof(Node*) * pr.many.count);
                if (!n.fn_call.args) {
                    err("Failed to allocate memory for args using arena");
                    return pr_fail();
                }
                memcpy(n.fn_call.args, pr.many.nodes,
                       sizeof(Node*) * pr.many.count);
                n.fn_call.args_count = pr.many.count;
            } else if (current(pctx).type != TokenCloseParen) {
                return expected_got("\"(\"", current(pctx));
            }
            consume(pctx); // ")"
            term = arena_add_node(pctx->ast->arena,n);
        }
    }
    return pr_ok(term);
}

ParseRes parse_unary(ParserCtx* pctx) {
    Node* term;
    if (    current(pctx).type == TokenStar
         || current(pctx).type == TokenAmpersand
         || current(pctx).type == TokenMinus
         || current(pctx).type == TokenTilde
         || current(pctx).type == TokenBang
    ) {
        Token op = consume(pctx); // unary op
        term = parse_unary(pctx).node;
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
        term = arena_add_node(pctx->ast->arena, n);
    } else {
        term = parse_postfixes(pctx).node;
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

ParseRes prec_climbing(ParserCtx* pctx, int min_prec) {
    if (min_prec <= 0) min_prec = 1;
    Node* lhs = parse_unary(pctx).node;
    if (!lhs) {
        err("Failed to parse term.\n");
        return pr_fail();
    }

    if (current(pctx).type == TokenCloseParen) {
        return pr_ok(lhs);
    }

    while (get_precedence(get_op(current(pctx))) >= min_prec) {
        if (current(pctx).type == TokenSemicolon) {
            err("For some fuckas reasong semicolon triggered a reparsing");
            assert(0);
        }
        OpType op = get_op(consume(pctx));
        int current_prec = get_precedence(op);

        // aslways left associativity?
        // right association would be next min prec = min prec
        // use current(pctx) prec since if min prec is 1 and prec of current(pctx) op
        // is 5 (for examoke), min prec + 1 would be 2, so some ops less than 5
        // might be parsed first
        // assignment must be right associative
        Node* rhs;
        // right associativity: a = b = c -> a = (b=c)
        if (current_prec == get_precedence(OpAssign)) 
            rhs = prec_climbing(pctx, current_prec).node;
        else // next one for left associativity: a = b = c -> (a=b) = c
            rhs = prec_climbing(pctx, current_prec + 1).node;
        if (!rhs) {
            err("Failed to parse expression in expression");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = op;
        n.binop.left = lhs;
        n.binop.right = rhs;
        lhs = arena_add_node(pctx->ast->arena, n);
    }

    return pr_ok(lhs);
}

ParseRes parse_expression(ParserCtx* pctx) {
    return prec_climbing(pctx, 1); // 0 is invalid
}

ParseRes parse_type(ParserCtx* pctx) {
    if (current(pctx).type != TokenIdent) {
        return expected_got("identifier", consume(pctx));
    }
    Token type_ident = consume(pctx);
    type_t t;
    type_t* ptr = get_type_from_name(type_ident.ident);
    if (!ptr) {
        char buf[100];
        err("unknown type \"%s\".",
            print_name_to_buf(buf, 100, type_ident.ident));
        return pr_fail();
    }
    t = *get_type_from_name(type_ident.ident);
    while (current(pctx).type == TokenStar) {
        consume(pctx); // it's a pointer
        type_t ptr;
        ptr.t = ptr_t;
        ptr.ptr = arena_alloc(&pctx->gpa,sizeof(type_t));
        if (!ptr.ptr) {
            err("Failed to allocate memory in termporary arena.");
            return pr_fail();
        }
        *(ptr.ptr) = t;
        t = ptr;
    }
    // type_t print_t = t;
    // printf("parsed typee: "); while (print_t.ptr != NULL) {
    //     printf("pointer to ");
    //     print_t = *print_t.ptr;
    // }
    // Name* vptr = get_name_from_type(print_t);
    // printf("%s\n",vptr->name);
    Node n;
    n.type = _NodeType;
    n._type = t;
    return pr_ok(arena_add_node(pctx->ast->arena, n));
}

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
        if (type_node->type != _NodeType) {
            err("Exptected type node, but got something else"
                "(which is completely wrong)");
            return pr_fail();
        }
        var_dec_node->var_dec.type = type_node->_type;
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

    if (fn_name.type != TokenIdent) {
        return expected_got("function name",current(pctx));
    }
    if (current(pctx).type != TokenOpenParen) {
        return expected_got("\"(\"", current(pctx));
    }
    consume(pctx); // "("
    // parse args
    if (current(pctx).type != TokenCloseParen) {
        return expected_got("\")\"", current(pctx));
    }
    consume(pctx); // ")"
    // parse return type
    if (current(pctx).type == TokenMinus && peek(pctx).type == TokenGreater) {
        consume(pctx);
        consume(pctx);
        Node* type_ptr = parse_type(pctx).node;
        if (!type_ptr) {
            err("Failed to parse fn return type.");
            return pr_fail();
        }
        if (type_ptr->type != _NodeType) {
            err("Exptected type node, got something else"
                "(shouldn't happend at all btw).");
            return pr_fail();
        }
        fn_dec.fn_dec.return_type = type_ptr->_type;
    } else {
        fn_dec.fn_dec.return_type = (type_t){.t = void_t};
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
            TODO("implement if");
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

    /* return */  expected_got("a valid statement", current(pctx)); // TODO make this make sense
    consume(pctx); // ivalid
    return pr_fail();
}
ParseRes parse_block_statement(ParserCtx* pctx) {
    if (current(pctx).type != TokenOpenBrace) {
        return expected_got("\"{\"", current(pctx));
    }
    consume(pctx); // "{"
    
    Node block;
    block.type = NodeBlock;
    Node** block_statements = arena_alloc(pctx->ast->arena, 100*sizeof(Node*));
    size_t block_index = 0;
    while (current(pctx).type != TokenCloseBrace) {
        // parse_statement
        ParseRes pr = parse_statement(pctx);
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
    consume(pctx); // "{"
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
            print_node(expr, 10);
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

