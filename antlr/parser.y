%{
#include <stdio.h>
#include <stdlib.h>
#include "node.h"

int yylex();
void yyerror(const char *s);
typedef enum {
    NodeBinop,
    NodeNum,
} NodeKind;
struct Node {
    NodeKind kind;
    union {
        struct {
            char op;
            Node *left, *right;
        } binop;
        int num;
    };
};
Node* new_num(int num) {
    Node* n = calloc(1, sizeof(Node));
    n->kind = NodeNum;
    n->num = num;
    return n;
}
Node* new_binop(char op, Node* left, Node* right) {
    Node* n = calloc(1, sizeof(Node));
    n->kind=NodeBinop;
    n->binop.op = op;
    n->binop.left = left;
    n->binop.right = right;
    return n;
}
Node* root = NULL;
%}


%union {
    int intval;
    void* node;
}
%token <intval> INT
%token PLUS MINUS MUL DIV

%type <node> expr
%type <node> program

%left PLUS MINUS
%left MUL DIV

%%

program:
    expr {
        root = $1;
        $$ = $1;
    }
;
expr:
      expr PLUS expr   { $$ = new_binop('+', $1, $3);}
    | expr MINUS expr  { $$ = new_binop('-', $1, $3);}
    | expr MUL expr    { $$ = new_binop('*', $1, $3);}
    | expr DIV expr    { $$ = new_binop('/', $1, $3);}
    | INT              { $$ = new_num($1); }
    ;

%%

int main() {
yyparse();
    printf("%zu\n", root);
    return 0; 
}

void yyerror(const char *s) {
    fprintf(stderr, "error: %s\n", s);
}
