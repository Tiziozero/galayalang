%{
#include "parser.tab.h"
#include "node.h"
%}

%%

%union {
    int intval;
    Node* node;
}

[0-9]+ {
    yylval.intval = atoi(yytext); // get it's intval
    return INT;
}
[ \t\n]+    { /* skip whitespace */ }

"+"         { return PLUS; }
"-"         { return MINUS; }
"*"         { return MUL; }
"/"         { return DIV; }

.           { return yytext[0]; }

%%
