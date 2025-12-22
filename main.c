#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
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
    ::= "let" name [ "=" expression ] ";" ;
fn_decl
    ::= "fn" name "(" [ param_list ] ")" [ return_type ] block ;
param_list
    ::= param { "," param } ;
param
    ::= name ":" type ;
return_type
    ::= ":" type ;
block
    ::= "{" { statement } "}" ;
statement
    ::= var_decl
     |  expression_stmt
     |  block ;

expression_stmt
    ::= expression ";" ;


// for C-like assignments that return a value#
expression ::= assignment
     |  binary_expr ;

assignment ::= unary [ "=" expression ] ;
binary_expr ::= unary { op unary } ;

primary
    ::= name
     | number
     | "(" expression ")" ;
postfix
    ::= primary { fn_call | index } ;

unary
    ::= ("*" | "&" | "-" | "!") unary
     |  postfix ;

term ::= unary
fn_call
    ::= "(" [ argument_list ] ")" ;
index
    ::= "[" expression "]"

op -> tofinish
    ::= "+" | "-" | "*" | "&" | "&&" | "||"
     |  "%" | "^" | "=" | "|" | "<<" | ">>" | "!=" 
     |  "<" | ">" | "<=" | ">=" | "=="

argument_list
    ::= expression { "," expression } ;
type ::= name
name ::= IDENTIFIER ;

// lexer
IDENTIFIER ::= [a-zA-Z_][a-zA-Z0-9_]*

*/

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
    buf[length] = '\0';

    Lexer* l = lexer(buf, length);
    if (!l) {
        err( "Failed to lexe (?) file.");
        free(f);
        return 1;
    }

    Arena a = arena_new(1024, sizeof(Node));
    AST* ast = parse(l, &a);
    if (ast == NULL) {
            err( "Fauled to parse Tokens.");
            return 1;
    }

    if (!code_gen(ast)) {
        err("Couldn't generate code.");
        status = 1;
    } else {
        info("Code gen successful");
    }

    for (size_t i = 0; i < a.pages_count; i++) {
        free(a.pages[i]);
    }
    free(a.pages);
    free(ast->nodes); // free nodes
    free(ast); // then AST
    free(l->tokens);
    free(l);
    fclose(f);
    info("freed all");
    return status;
}

