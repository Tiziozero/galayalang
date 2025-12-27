#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "logger.h"
#include "lexer.h"
#include "parser.h"
#include "code_gen.h"


/*
program
    ::= { top_level_decl } ;
top_level_decl
    ::= var_decl
     |  fn_decl ;

var_decl
    ::= "let" IDENTIFIER [ "=" expression ] ";" ;
fn_decl
    ::= "fn" IDENTIFIER "(" [ param_list ] ")" [ return_type ] block ;

param_list
    ::= param { "," param } ;
param
    ::= IDENTIFIER ":" type ;
return_type
    ::= "->" type ;

block
    ::= "{" { statement } "}" ;

statement
    ::= var_decl
     |  fn_decl
     |  expression_stmt
     |  "return" [ expression ] ";"
     |  block ;

expression_stmt
    ::= expression ";" ;

expression ::= assignment ;
assignment ::= unary "=" assignment | binary_expr ;
binary_expr ::= unary { op unary } ;

binary_expr ::= unary { op unary } ;

primary
    ::= IDENTIFIER
     | NUMBER
     | "(" expression ")" ;
postfix
    ::= primary { fn_call | index } ;

unary
    ::= ("*" | "&" | "-" | "!") unary
     |  postfix ;

fn_call
    ::= "(" [ argument_list ] ")" ;
index
    ::= "[" expression "]" ;

op  ::= "+" | "-" | "*" | "&" | "&&" | "||"
     |  "%" | "^" | "=" | "|" | "<<" | ">>" | "!=" 
     |  "<" | ">" | "<=" | ">=" | "==" ;

argument_list
    ::= expression { "," expression } ;

type ::= IDENTIFIER [ { "*" | "[" NUMBER "]" } ] ;

// lexer
IDENTIFIER ::= [a-zA-Z_][a-zA-Z0-9_]*
NUMBER ::= [0-9]+(\.[0-9]+)?|[0-9]*\.[0-9]+


*/

// TODO: refactor expression to this
// add precedence level to grammar
int main(int argc, char** argv) {
    int status = 0;
    if (argc < 2) {
        err( "Expected arguments.\nUsage: <program> <file>\n");
        return 1;
    }
    // read file
    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        err( "Couldn't open file %s", argv[1]);
        return 1;
    }
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);
    char buf[1024*1024];
    fread(buf, 1, sizeof(buf), f);
    fclose(f); // free file
    buf[length] = '\0';

    Lexer* l = lexer(buf, length);
    if (!l) {
        err( "Failed to lex (?) file.");
        free(f);
        return 1;
    }

    ParserCtx* pctx = parse(l);
    if (pctx == NULL) {
            err("Failed to parse Tokens.");
            return 1;
    }

    if (!code_gen(pctx->ast)) {
        err("Couldn't generate code.");
        status = 1;
    } else {
        info("Code gen successful");
    }

    if (!pctx_destry(pctx)) {
        err("Failed to free parser context");
    }
    free(l->tokens);
    free(l);
    info("freed all");
    return status;
}

