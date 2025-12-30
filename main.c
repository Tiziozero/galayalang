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
     |  if_stmt
     |  expression_stmt
     |  return_stmt
     |  block ;


return_stmt
    ::= "return" [ expression ] ";" ;

expression_stmt
    ::= expression ";" ;

if_stmt
    ::= ( "if" expression block 
    { "else if" expression block }
    [ "else" block ] )
    | ( "if" expression ( expression_stmt | return_stmt ) ) ;


expression      ::= assignment_expr { "," assignment_expr };
assignment_expr ::= lvalue assignment_op assignment_expr
                 |  conditional_expr ;

conditional_expr::= logical_or      [ "?" expression ":" conditional_expr ] ;
logical_or      ::= logical_and     { "||"  logical_and };
logical_and     ::= bitwise_or      { "&&"  bitwise_or  } ;
bitwise_or      ::= bitwise_xor     { "|"   bitwise_xor } ;
bitwise_xor     ::= bitwise_and     { "&"   bitwise_and } ;
bitwise_and     ::= logical_comp    { "&"   logical_comp } ;
logical_comp    ::= relational      { ("==" | "!=" ) relational} ;
relational      ::= bit_shift       {  ">=" | "<=" | "<" | ">" ) bit_shift } ;
bit_shift       ::= additive        { ("<<" | ">>") additive } ;
additive        ::= multiplicative  { ("+" | "-") multiplicative } ;
multiplicative  ::= unary       { ("*" | "/" | "%") unary } ;

unary           ::= ("*" | "&" | "-" | "!" | "~") unary
                 |  cast_expr ;
cast_expr       ::= "(" type ")" cast_expr
                 |  postfix ;

lvalue          ::= IDENTIFIER
                 |  primary index
                 |  "*" postfix;


postfix         ::= primary { fn_call | index | "." IDENTIFIER } ;

primary         ::= IDENTIFIER
                 |  NUMBER
                 |  "(" expression ")" ;

assignment_op   ::= ( "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" )
fn_call
    ::= "(" [ argument_list ] ")" ;
index
    ::= "[" expression "]" ;

argument_list
    ::= expression { "," expression } ;

type ::= IDENTIFIER { "*" | "[" NUMBER "]" } ;

// lexer
IDENTIFIER ::= [a-zA-Z_][a-zA-Z0-9_]*
NUMBER ::= [0-9]+(\.[0-9]+)?|[0-9]*\.[0-9]+
*/

// TODO: refactor expression to this
// add precedence level to grammar
int main(int argc, char** argv) {
    int status = 0;
	char* path;
    if (argc < 2) {
        // err( "Expected arguments.\nUsage: <program> <file>\n");
        // return 1;
		path = "main.gala";
    } else {
		path = argv[1];
	}
    // read file
    FILE* f = fopen(path, "rb");
    if (!f) {
        err( "Couldn't open file %s", path);
        return 1;
    }
    dbg("opened file");
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);
    dbg("got end");
    char buf[1024*1024];
    fread(buf, 1, sizeof(buf), f);
    dbg("read file");
    fclose(f); // free file
    dbg("closed file");
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
    return status;
}

