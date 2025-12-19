#include "parser.h"
#include "lexer.h"
#include "logger.h"
#include "utils.h"
// 1 is success, 0 faulure

#include "parse_number.c"
// term { "+" term }
Node* parse_expression(AST* ast, Token* tokens, size_t* i, size_t len); 
// number | "(" expression ")"
Node* parse_term(AST* ast, Token* tokens, size_t* i, size_t len); 
int parse_top_level_statement(AST* ast, Token* tokens, size_t* i, size_t len);
Node* parse_assignment(AST* ast, Token* tokens, size_t* i, size_t len, Name* target);
Node* parse_block_statement(AST* ast, Token* tokens, size_t* i, size_t len);


const char* node_type_to_string(NodeType t)
{
    switch (t) {
        case NodeNone:            return "None";
        case NodeNumLit:          return "NumberLiteral";
        case NodeAssignment:      return "Assignment";
        case NodeAddition:        return "Addition";
        case NodeSubtraction:     return "Subtraction";
        case NodeMultiplication:  return "Multiplication";
        case NodeDividion:        return "Division";
        case NodeVarDec:          return "VarDeclaration";
        default:                  return "<unknown>";
    }
}

void print_node(const Node* n) {
    if (!n) {
        printf("<null node>\n");
        return;
    }

    printf("%s: ", node_type_to_string(n->type));

    switch (n->type) {
        case NodeVarDec:
            print_name(n->var_dec);
            break;

        case NodeNumLit:
            printf("%lf\n", n->number);
            break;

        case NodeAssignment:
            printf("target=");
            write_name(n->assignment.target);
            printf(", value=<node %p> or ", (void*)n->assignment.value);
            print_node(n->assignment.value);

            break;
        case NodeAddition:
            // write_name(n->additi)

        case NodeNone:
            printf("\n");
            break;

        default:
            printf("<unhandled>\n");
            break;
    }
}
void print_ast(const AST* ast)
{
    if (!ast) {
        err("(null ast)");
        return;
    }

    info("AST (%zu nodes)", ast->nodes_count);
    printf("=== AST ===\n");

    for (size_t i = 0; i < ast->nodes_count; i++) {
        printf("  [%zu] ", i);
        print_node(ast->nodes[i]);
    }
}


int ast_add_node(AST* ast, Node* n) {
    ast->nodes[ast->nodes_count++] = n;
    if (ast->nodes_count >= ast->max_nodes) {
        ast->max_nodes *= 2;
        ast->nodes = realloc(ast->nodes, ast->max_nodes);
        if (ast->nodes == NULL) {
            err( "Failed to reallocate memory for ast.");
            exit(1);
            return 1;
        }
    }
    return 0;
}

Node* arena_add_node(Arena* a, Node n) {
    return arena_add(a, sizeof(Node), &n);
}
AST* parse(Lexer* l, Arena* a) {
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->max_nodes=1024;
    ast->nodes = (Node**)malloc(sizeof(Node*)*ast->max_nodes);
    ast->nodes_count = 0;
    ast->arena = a;

    Token* tokens = l->tokens;
    size_t len = l->tokens_count;

    // rules
    /*
        *   "let" name { "=" expression } ";" -- will be of size_t
        *   "fn" name "(" { args } ")" "{" block "}" 
        *   
        *   block -> statements
        */
    #define current tokens[i]
    #define peek tokens[i+1]
    #define consume tokens[i++]
    size_t i = 0;
    while (i < len && current.type != TokenEOF) {
        // info("\tCurrent: %s:\n", get_token_type(current.type));
        if (!parse_top_level_statement(ast, tokens, &i, len)) {
            err( "Failed to parse top level statement.");
            free(ast->nodes);
            free(ast);
            return NULL;
        }
    }

    #undef consume
    #undef peek
    #undef current
    print_ast(ast);
    return ast;
}
#define current tokens[*i]
#define peek tokens[(*i) + 1]
#define consume tokens[(*i)++]
Node* parse_term(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node n;
    if (current.type == TokenNumber) {
        double out;
        if (!parse_number(current.number.name, current.number.length, &out)) {
            err( "Failed to parse number.\n");
            return NULL;
        }
        consume; // number

        Node node;
        node.type = NodeNumLit;
        node.number = out;
        Node* ret_n = arena_add_node(ast->arena,node);
        return ret_n;
    } else if (0) {

    } else {
        err( "Expected number, gor: %s.\n", get_token_type(current.type));
        return NULL;
    }
}
Node* parse_expression(AST* ast, Token* tokens, size_t* i, size_t len) {
    Node* term = parse_term(ast, tokens, i, len);
    if (!term) {
        err("Failed to parse term.\n");
    }
    // ast_add_node(ast, term);
    if (current.type == TokenPlus) {
        consume;
        Node* rigth_expr = parse_expression(ast, tokens, i, len);
        Node add_node;
        add_node.type = NodeAddition;
        add_node.addition.left = term;
        add_node.addition.right = rigth_expr;
        Node* add_node_ptr = arena_add_node(ast->arena,add_node);
        return add_node_ptr;
    }
    return term;
}

int parse_top_level_let(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume;
    if (current.type != TokenIdent) {
        err(
            "Expected identifier after \"let\", got: %s.\n",
            get_token_type(current.type));
        return 0;
    }
    // get name
    Name identifier = consume.ident;
    info("new var: ");
    write_name(identifier);
    // allocate and add to ast
    Node n;
    n.type = NodeVarDec;
    n.var_dec = identifier;
    ast_add_node(ast, arena_add_node(ast->arena, n));
    info("Next token aver name: %s.", get_token_type(current.type));

    // if it's a semicolon then just declare eg: let i;
    if (current.type == TokenSemicolon) {
        consume;
    } else if (current.type == TokenAssign) {
        // else if it's an assignment the parse expression
        consume; // consume assign
        // parse term. top level can't have expression?
        Node* expr = parse_expression(ast, tokens, i, len);
        if (expr->type != NodeNumLit) {
            err("top level statement must have a compile time consttant. A number literal. Got %s.", node_type_to_string(expr->type));
            return 0;
        }
        if (current.type == TokenSemicolon) { 
            consume;
            // assignment node
            Node assignment;
            assignment.type = NodeAssignment;
            // target is identifier;
            assignment.assignment.target = identifier;
            assignment.assignment.value = expr;
            ast_add_node(ast,arena_add_node(ast->arena, assignment));
            // if it's not a semicolon then invalid syntax.
            // max one expression per assignment
        } else {
            err("expected \";\" after expression, got: %s.",
                get_token_type(current.type));
            return 0;
        }
        // add type here
    } else {
        //err(
        err("Expected \";\", type or assignment after variable name,"
            "but got: %s.", get_token_type(current.type));
        return 0;
    }

    return 1;
}
int parse_let(AST* ast, Token* tokens, size_t* i, size_t len) {
    consume; // let
    if (current.type != TokenIdent) {
        err(
            "Expected identifier after \"let\", got: %s.\n",
            get_token_type(current.type));
        return 0;
    }
    // get name
    Name identifier = consume.ident;
    info("new var: ");
    write_name(identifier);
    // allocate and add to ast
    Node n;
    n.type = NodeVarDec;
    n.var_dec = identifier;
    ast_add_node(ast, arena_add_node(ast->arena, n));
    info("Next token aver name: %s.", get_token_type(current.type));

    // if it's a semicolon then just declare eg: let i;
    if (current.type == TokenSemicolon) {
        consume;
    } else if (current.type == TokenAssign) {
        // else if it's an assignment the parse expression
        consume; // consume assign
        // parse expression. block assignment can be everything
        Node* expr = parse_expression(ast, tokens, i, len);
        if (current.type == TokenSemicolon) { 
            consume;
            // assignment node
            Node assignment;
            assignment.type = NodeAssignment;
            // target is identifier;
            assignment.assignment.target = identifier;
            assignment.assignment.value = expr;
            ast_add_node(ast,arena_add_node(ast->arena, assignment));
            // if it's not a semicolon then invalid syntax.
            // max one expression per assignment
        } else {
            err("expected \";\" after expression, got: %s.",
                get_token_type(current.type));
            return 0;
        }
    // add type here
    } else if (current.type == TokenColon) {
    } else {
        err("Expected \";\", type or assignment after variable name,"
            "but got: %s.", get_token_type(current.type));
        return 0;
    }

    return 1;
}

int parse_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    // info("Got: %s", get_token_type(current.type));
    if (current.type == TokenKeyword) {
        if (current.kw == KwLet) {
            return parse_let(ast, tokens, i, len);
        } else if (current.kw == KwFn) {

        } else if (current.kw == KwIf) {

        } else if (current.kw == KwStruct) {

        } else {
            err("Invalid keyword: %s.\n", get_keyword_name(current.kw));
            return 0;
        }
    } else {
        err("expect something valid, got: %s", get_token_type(current.type));
        return 0;
    }

    return 1;
}
Node* parse_block_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type != TokenOpenBrace) {
        err("Expected \"{\" for block statement, got: %s",
            get_token_type(current.type));
        return NULL;
    }
    consume; // "{"
    
    while (current.type != TokenCloseBrace) {
        // parse_statement
        parse_statement(ast, tokens, i,  len);
    }
    consume; // "{"

    return NULL;
}

int parse_fn(AST* ast, Token* tokens, size_t* i, size_t len) {
    info("Got function.");
    consume; // eat fn
    Token fn_name = consume;
    if (fn_name.type != TokenIdent) {
        err("Expected name, got: %s", get_token_type(fn_name.type));
        return 0;
    }
    info("Function name:");print_name(fn_name.ident);
    if (current.type != TokenOpenParen) {
        err("Expected \"(\", got: %s", get_token_type(current.type));
        return 0;
    }
    consume; // "("
    // parse args
    if (current.type != TokenCloseParen) {
        err("Expected \")\", got: %s", get_token_type(current.type));
        return 0;
    }
    consume; // ")"
    // parse return type
    //
    // expect for block
    if (current.type != TokenOpenBrace) {
        err("Expected \"{\", got: %s", get_token_type(current.type));
        return 0;
    }
    // consume "{" and "}" in parse block
    // parse block statement
    Node* fn_body = parse_block_statement(ast, tokens, i, len);

    Node fn_dec;

    return 1;
}

// return 1 on succecss
int parse_top_level_statement(AST* ast, Token* tokens, size_t* i, size_t len) {
    if (current.type == TokenSemicolon) {
        consume;
        return 1; // ok ig?
    }
    // only kw for now
    if (current.type != TokenKeyword) {
        err( "Expected keyword, got: %s.\n", get_token_type(current.type));
        return 0;
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
        return 0;
    }
}

Node* parse_assignment(AST* ast, Token* tokens, size_t* i, size_t len, Name* target) {

    return NULL;
}


#undef consume
#undef peek
#undef current

