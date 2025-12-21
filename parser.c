#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
#include "parse_number.c"
#include <assert.h>
#include <stddef.h>


int is_lvalue(Node* node) {
    return
    node->type == NodeDereference ||
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
            err("Failed to parse expression.");
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
        consume; // number

        Node node;
        node.type = NodeNumLit;
        node.number.number = out;
        return pr_ok(arena_add_node(ast->arena,node));
    } else if (current.type == TokenIdent) {
        Name ident = consume.ident;
        // print_name(ident);
        // info("Ident: %.*s\n", (int)ident.length, ident.name);
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

    // reference
    } else if (current.type == TokenAmpersand) {
        consume;
        // reference must be a term like var or (...)
        ParseRes pr = parse_term(ast, tokens, i, len);
        if (pr.ok != PrOk) {
            err("Faied to parse term for dereference.");
            return pr_fail();
        }
        Node n;
        n.type = NodeReference;
        n.ref.target = pr.node;
        return pr_ok(arena_add_node(ast->arena, n));
    // dereference
    } else if (current.type == TokenStar) {
        consume;
        // dereference must be a term like var or (...)
        ParseRes pr = parse_term(ast, tokens, i, len);
        if (pr.ok != PrOk) {
            err("Faied to parse term for dereference.");
            return pr_fail();
        }
        Node n;
        n.type = NodeDereference;
        n.deref.target = pr.node;
        return pr_ok(arena_add_node(ast->arena, n));
    // TODO add shit here
    } else if (0) {

    } else {
        err( "Expected number, got: %s.\n", get_token_type(current.type));
        return pr_fail();
    }
    return pr_fail();
}
// next level * and / and %
ParseRes parse_factor(AST* ast, Token* tokens, size_t* i, size_t len) {
    ParseRes pr = parse_term(ast, tokens, i, len);
    if (pr.ok != PrOk) {
        err("Failed to parse term.");
        return pr_fail();
    }
    if (current.type == TokenStar) {
        consume; // *
        ParseRes next = parse_term(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpMlt;
        n.binop.left = pr.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    } else if (current.type == TokenSlash) {
        consume; // /
        ParseRes next = parse_term(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpDiv;
        n.binop.left = pr.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    } else if (current.type == TokenPercent) {
        consume; // /
        ParseRes next = parse_term(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpMod;
        n.binop.left = pr.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    }
    return pr;
}
// next level + and -
ParseRes parse_addsub(AST* ast, Token* tokens, size_t* i, size_t len) {
    ParseRes factor = parse_factor(ast, tokens, i, len);
    if (factor.ok == PrFail) {
        err("Failed to parse factor.");
        return pr_fail();
    }
    if (current.type == TokenPlus) {
        consume; // +
        ParseRes next = parse_factor(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpAdd;
        n.binop.left = factor.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    } else if (current.type == TokenMinus) {
        consume; // -
        ParseRes next = parse_factor(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpSub;
        n.binop.left = factor.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    }
    return factor;
}
// next level >> and <<
ParseRes parse_shift(AST* ast, Token* tokens, size_t* i, size_t len) {
    ParseRes addsub = parse_addsub(ast, tokens, i, len);
    if (addsub.ok == PrFail) {
        err("Failed to parse addsub.");
        return pr_fail();
    }
    if (current.type == TokenShiftL) {
        consume; // <<
        ParseRes next = parse_addsub(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpLSh;
        n.binop.left = addsub.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    } else if (current.type == TokenShiftR) {
        consume; // >>
        ParseRes next = parse_addsub(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpRSh;
        n.binop.left = addsub.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    }
    return addsub;
}
// next level &
ParseRes parse_bw_and(AST* ast, Token* tokens, size_t* i, size_t len) {
    ParseRes shift = parse_shift(ast, tokens, i, len);
    if (shift.ok == PrFail) {
        err("Failed to parse shift.");
        return pr_fail();
    }
    if (current.type == TokenAmpersand) {
        consume; // &
        ParseRes next = parse_shift(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpAnd;
        n.binop.left = shift.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    }
    return shift;
}
// next level ^
ParseRes parse_bw_xor(AST* ast, Token* tokens, size_t* i, size_t len) {
    ParseRes bw_and = parse_bw_and(ast, tokens, i, len);
    if (bw_and.ok == PrFail) {
        err("Failed to parse bw_and.");
        return pr_fail();
    }
    if (current.type == TokenCaret) {
        consume; // ^
        ParseRes next = parse_bw_and(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpXor;
        n.binop.left = bw_and.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    }
    return bw_and;
}
// next level |
ParseRes parse_bw_or(AST* ast, Token* tokens, size_t* i, size_t len) {
    ParseRes bw_xor = parse_bw_xor(ast, tokens, i, len);
    if (bw_xor.ok == PrFail) {
        err("Failed to parse bw_xor.");
        return pr_fail();
    }
    if (current.type == TokenPipe) {
        consume; // |
        ParseRes next = parse_bw_xor(ast, tokens, i, len);
        if (next.ok != PrOk) {
            err("Failed to parse term after \"*\".");
            return pr_fail();
        }
        Node n;
        n.type = NodeBinOp;
        n.binop.type = OpOr;
        n.binop.left = bw_xor.node;
        n.binop.right = next.node;
        return pr_ok(arena_add_node(ast->arena, n));
    }
    return bw_xor;
}
// && and ||
ParseRes parse_expression(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node* bw_or = parse_bw_or(ast, tokens, i, len).node;
    if (!bw_or) {
        err("Failed to parse term.\n");
    }
    if (current.type == TokenCloseParen) {
        return pr_ok(bw_or);
    }

    // ast_add_node(ast, term);
    if (current.type == TokenAndAnd) {
        consume;
        Node* rigth_expr = parse_expression(ast, tokens, i, len).node;
        if (!rigth_expr) {
            err("Failed to parse expression.");
            return pr_fail();
        }
        Node n;
        n.type = NodeAndAnd;
        n.binop.left = bw_or;
        n.binop.right = rigth_expr;
        Node* add_node_ptr = arena_add_node(ast->arena,n);
        return pr_ok(arena_add_node(ast->arena,n));
    }
    if (current.type == TokenOrOr) {
        consume;
        Node* rigth_expr = parse_expression(ast, tokens, i, len).node;
        if (!rigth_expr) {
            err("Failed to parse expression.");
            return pr_fail();
        }
        Node n;
        n.type = NodeOrOr;
        n.binop.left = bw_or;
        n.binop.right = rigth_expr;
        return pr_ok(arena_add_node(ast->arena,n));
    }
    return pr_ok(bw_or);
}

ParseRes parse_top_level_let(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume;
    if (current.type != TokenIdent) {
        err(
            "Expected identifier after \"let\", got: %s.\n",
            get_token_type(current.type));
        return pr_fail();
    }
    // get name
    Name identifier = consume.ident;
    // allocate var declaration node
    Node n;
    n.type = NodeVarDec;
    n.var_dec.name = identifier;
    Node* var_dec_node = arena_add_node(ast->arena, n);

    // if it's a semicolon then just declare eg: let i;
    if (current.type == TokenSemicolon) {
        consume;
    } else if (current.type == TokenAssign) {
        // else if it's an assignment the parse expression
        consume; // consume assign
        // parse term. top level can't have expression?
        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (expr->type != NodeNumLit) {
            err("top level statement must have a compile time consttant. A number literal. Got %s.", node_type_to_string(expr->type));
            return pr_fail();
        }
        if (current.type == TokenSemicolon) { 
            consume;
            // assignment node
            Node assignment;
            assignment.type = NodeAssignment;
            // target is identifier;
            Node var;
            var.type = NodeVar;
            var.var.name = identifier;
            assignment.assignment.target = arena_add_node(ast->arena, var);
            assignment.assignment.value = expr;
            Node* assignment_node = arena_add_node(ast->arena, assignment);

            ParseRes pr;
            pr.ok = PrMany;
            pr.many.nodes[0] = var_dec_node;
            pr.many.nodes[1] = assignment_node;
            pr.many.count = 2;
            return pr;
            // if it's not a semicolon then invalid syntax.
            // max one expression per assignment
        } else {
            err("expected \";\" after expression, got: %s. %zu:%zu",
                get_token_type(current.type), current.line, current.col);
            consume;
            return pr_fail();
        }
        // add type here
    } else {
        //err(
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
    Name identifier = consume.ident;
    // allocate var declaration node
    Node n;
    n.type = NodeVarDec;
    n.var_dec.name = identifier;
    Node* var_dec_node = arena_add_node(ast->arena, n);

    // if it's a semicolon then just declare eg: let i;
    if (current.type == TokenSemicolon) {
        consume;
    } else if (current.type == TokenAssign) {
        // else if it's an assignment the parse expression
        consume; // consume assign
        // parse expression. block assignment can be everything
        Node* expr = parse_expression(ast, tokens, i, len).node;
        if (current.type == TokenSemicolon) { 
            consume;
            // assignment node
            Node assignment;
            assignment.type = NodeAssignment;
            // target is identifier;
            Node var;
            var.type = NodeVar;
            var.var.name = identifier;
            assignment.assignment.target = arena_add_node(ast->arena, var);
            assignment.assignment.value = expr;

            // allocate
            Node* assignment_node = arena_add_node(ast->arena, assignment);

            ParseRes pr;
            pr.ok = PrMany;
            pr.many.nodes[0] = var_dec_node;
            pr.many.nodes[1] = assignment_node;
            pr.many.count = 2;
            return pr;
            // if it's not a semicolon then invalid syntax.
            // max one expression per assignment
        } else {
            err("expected \";\" after expression, got: %s.",
                get_token_type(current.type));
            return pr_fail();
        }
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

ParseRes parse_assignmet_expression(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node* expr = parse_expression(ast, tokens, i, len).node;
    if (!expr) {
        consume; // invalid token
        err("Failed to parse expression");
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
}
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
        Node* expr = parse_assignmet_expression(ast, tokens, i, len).node;
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
