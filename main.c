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
    ::= "if" if_condition if_block 
           { "else if" if_condition if_block }
           [ "else" if_block ] ;

if_condition ::= expression ;
if_block
    ::=    expression_stmt
     |    return_stmt
     |    block;

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
relational      ::= bit_shift       { (">=" | "<=" | "<" | ">" ) bit_shift } ;
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

array_type ::= "[" number "]" | "[" "dyn" "]" | "[]"; // fixed/dynamic/slice
pointer_type ::= "*" ;
type_identifier ::= IDENTIFIER ;
type_prefix ::= array_type | pointer_type ;
type_atom ::= type_identifier | "(" type ")" ;
type
    ::= type_prefix* type_atom;

// lexer
IDENTIFIER ::= [a-zA-Z_][a-zA-Z0-9_]*
NUMBER ::= [0-9]+(\.[0-9]+)?|[0-9]*\.[0-9]+
*/
/*
 * Types:
 *  Numbers:
 *      signed:
 *          i8, i16, i32, i64, i128
 *      unsigned:
 *          u8, u16, u32, u64, u128, ptr
 *      float:
 *          f32, f64
 *  Aggregate types:
 *      struct: "struct <name> { <field_name>: <type> };"
 *      enum:   "enum   <name> { <name> {optional  "= value"} };"
 * types are very strict: i32 + u32 is not valid unless casted.
 */

// TODO: refactor expression to this
// add precedence level to grammar
#include <stdlib.h>
#include <stddef.h>

char **split_lines(char *src, size_t *out_count) {
    size_t cap = 16;
    size_t count = 0;
    char **lines = malloc(cap * sizeof(char *));
    if (!lines) return NULL;

    char *p = src;
    lines[count++] = p;

    while (*p) {
        if (*p == '\r') {
            *p = '\0';
            if (p[1] == '\n')
                p++;            // swallow \n in \r\n
            p++;
            if (*p) {
                if (count == cap) {
                    cap *= 2;
                    lines = realloc(lines, cap * sizeof(char *));
                    if (!lines) return NULL;
                }
                lines[count++] = p;
            }
            continue;
        }

        if (*p == '\n') {
            *p = '\0';
            p++;
            if (*p) {
                if (count == cap) {
                    cap *= 2;
                    lines = realloc(lines, cap * sizeof(char *));
                    if (!lines) return NULL;
                }
                lines[count++] = p;
            }
            continue;
        }

        p++;
    }

    *out_count = count;
    return lines;
}
int main(int argc, char** argv) {
    printf("Log level %d", LOG_LEVEL);
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
    // create lines to print
    l->code = buf;
    char* code_copy = malloc(length*sizeof(char));
    memset(code_copy, 0, length*sizeof(char));
    memcpy(code_copy, buf, length*sizeof(char));
    size_t out = 0;

    char** lines = split_lines(code_copy, &out);
    if (!lines) panic("Failed to split code into lines");
    l->lines = lines;
    l->lines_count = out;


    ParserCtx* pctx = parse(l);
    if (pctx == NULL) {
            err("Failed to parse Tokens.");
            return 1;
    }

    if (!code_gen(pctx)) {
        err("Couldn't generate code.");
        status = 1;
    } else {
        info("Code gen successful");
    }

    info("End parser");
    if (!pctx_destry(pctx)) {
        err("Failed to free parser context");
    }
    info("freeing lexer");
    free(l->tokens);
    free(l);
    printf("Finished.\n");
    return status;
}

