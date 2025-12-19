#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "utils.h"
typedef struct {
    char* name;
    size_t length;
} Name;

Name new_name(char* name, size_t length) {
    return (Name){.name=name,.length=length};
}
// returns 1 if equal, 0 if not
int name_cmp(Name n1, Name n2) {
    if (n1.length != n2.length) return 0;

    for (size_t i = 0; i < n1.length; i++) {
        if (n1.name[i] != n2.name[i]) return 0;
    }
    return 1;
}

typedef enum {
    KwFn,
    KwLet,
    KwWhile,
    KwReturn,
    KwIf,
    KwElse,
    KwNone,
} KeyWord;
Name key_words[] = {
    {.name="fn", .length=2},
    {.name="let", .length=3},
    {.name="while", .length=5},
    {.name="return", .length=6},
    {.name="if", .length=2},
    {.name="else", .length=4},
};

typedef struct {
    TokenType type;
    size_t line, col;
    union {
        Name ident;
        KeyWord kw;
        Name number;
    };
} Token;

typedef struct {
    size_t max_tokens;
    size_t tokens_count;
    Token* tokens;
} Lexer;

Name get_keyword_name(KeyWord kw) {
    if (kw >= KwNone) return (Name){.name=0,.length=0};
    return key_words[kw];
}
KeyWord get_name_kw(Name n) {
    for (uint8_t i = 0; i < KwNone; i++) {
        if (name_cmp(n, key_words[i])) {
            return i;
        }
    }
    return KwNone;
}


typedef enum {
    NodeNone,
    NodeVarDec,
    NodeNumLit,
} NodeType;
typedef struct {
    NodeType type;
    union {
        Name var_dec; // change once type are implemented?
        double number;
    };
} Node;

typedef struct {
    Node** nodes; // array of nodes
    size_t nodes_count;
    size_t max_nodes;
} AST;
AST* parse(Lexer* l, Arena* a);




int lexer_add_token(Lexer* l, Token t) {
    l->tokens[l->tokens_count++] = t;
    if (l->tokens_count >= l->max_tokens) {
        l->max_tokens *= 2;
        l->tokens = realloc(l->tokens, l->max_tokens);
        if (l->tokens == NULL) {
            fprintf(stderr, "Failed to reallocate memory for lexer.\n");
            exit(1);
            return 1;
        }
    }
    return 0;
}

// returns 1 on true
int is_keyword(Name n1, Name* kws, size_t kwlen) {
    for (int i = 0; i < kwlen; i++) {
        if (name_cmp(n1, kws[i])) {
            return 1;
        }
    }
    return 0;
}

void print_ast(AST* ast) {
    if (!ast) {
        printf("(null ast)\n");
        return;
    }

    printf("AST (%zu nodes)\n", ast->nodes_count);

    for (size_t i = 0; i < ast->nodes_count; i++) {
        Node* n = ast->nodes[i];
        if (!n) {
            printf("  [%zu] <null node>\n", i);
            continue;
        }

        printf("  [%zu] ", i);

        switch (n->type) {
            case NodeVarDec:
                printf("VarDecl: ");
                fwrite(n->var_dec.name, 1, n->var_dec.length, stdout);
                printf("\n");
                break;

            case NodeNone:
                printf("None\n");
                break;

            default:
                printf("Unknown node type (%d)\n", n->type);
                break;
        }
    }
}

Lexer* lexer(char* buf, size_t size) {
    Lexer* l = malloc(sizeof(Lexer));
    l->max_tokens = 1024;
    l->tokens_count = 0;
    l->tokens  = (Token*)malloc(1024*sizeof(l->max_tokens));
    if (l->tokens == NULL) {
        fprintf(stderr, "Failed to allocate memory for lexer.\n");
        exit(1);
        return NULL;
    }



    size_t i = 0;
    size_t line = 0;
    size_t column = 0;

    while (i < size) {
        char c = buf[i];
        char peek = buf[i+1];

        if (c == '\n') {
            column = 0;
            line++;
            i++;
        } else if (c == ' ' || c == '\t') {
            column++;
            i++;
        } else if (c == EOF) {
            lexer_add_token(l, (Token){TokenEOF, line, column});
            break;
        }else if(c == '_' ||(c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ) {
            char* name_start = &buf[i];
            size_t len = 0;
            char cur = name_start[len];
            while(cur=='_'||(cur >= 'a'&&cur<='z')||(cur>='A'&&cur<='Z')||
                (cur>= '0' && cur <= '9')) {
                i++; len++; column++; // increment all
                cur = name_start[len];
            }
            // fwrite(name_start, 1, len, stdout);printf(" of len: %lu\n", len);
            Name n = {.name=name_start, .length=len};
            if(is_keyword(n,key_words,sizeof(key_words)/sizeof(key_words[0]))) {
                Token t;
                t.type = TokenKeyword;
                t.line = line;
                t.col = column;
                KeyWord kw = get_name_kw(n);
                if (kw == KwNone) {
                    n.name[n.length] = '\0';
                    fprintf(stderr, "Failed to get name from name %s.\n", n.name);
                    return NULL;
                }
                t.kw = get_name_kw(n);
                lexer_add_token(l, t);
            } else {
                Token t;
                t.type = TokenIdent;
                t.line = line;
                t.col = column;
                t.ident = n;
                lexer_add_token(l, t);
            }
        } else if (c >= '0' && c <= '9') {
            char* name_start = &buf[i];
            size_t len = 0;
            char cur = name_start[len];
            while((cur>= '0' && cur <= '9') || cur == '.') {
                 // increment all:i to next,len increses len,columnt for debug
                i++; len++; column++;
                cur = name_start[len];
            }
            // fwrite(name_start, 1, len, stdout);printf(" of len: %lu\n", len);
            Name n = {.name=name_start, .length=len};
            Token t;
            t.type = TokenNumber;
            t.number = n;
            lexer_add_token(l, t);
        } else if (is_double_symbol(c, peek) != TokenEOF) { // double symbols first
            lexer_add_token(l,(Token){
                .type=is_double_symbol(c, peek), .line=line,.col=column});
            column++;
            i++;i++; // eat two
        } else if (get_token_type_from_char(c) != TokenEOF) {
            lexer_add_token(l,(Token){
                .type=get_token_type_from_char(c), .line=line,.col=column});
            column++;
            i++;
        } else {
            fprintf(stderr, "Unknown kakapoopoo: %c\n", c);
            free(l);
            return NULL;
        }
    }

    return l;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Expected arguments.\nUsage: <program> <file>\n");
        return 1;
    }
    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        fprintf(stderr, "Couldn't open file %s\n", argv[1]);
        return 1;
    }
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);
    char buf[1024*1024];
    fread(buf, 1, sizeof(buf), f);
    buf[length] = '\0';
    printf("len, buf at len: %lu, %u\n", length, buf[length - 1]);
    printf("Got: \"%s\"\n", buf);


    Lexer* l = lexer(buf, length);
    if (!l) {
        fprintf(stderr, "Failed to lexe (?) file.\n");
        free(f);
        return 1;
    }
    for (int i = 0; i < l->tokens_count; i++) {
        Token t = l->tokens[i];
        printf("Token: %s\t\t",get_token_type(t.type));

        if (t.type == TokenIdent) {
            fwrite(t.ident.name, 1, t.ident.length, stdout);
        }
        if (t.type == TokenKeyword) {
            Name name = get_keyword_name(t.kw);
            fwrite(name.name, 1, name.length, stdout);
        }
        if (t.type == TokenNumber) {
            fwrite(t.number.name, 1, t.number.length, stdout);
        }
        printf("\n");
    }

    Arena a = arena_new(1024);
    AST* ast = parse(l, &a);
    if (ast == NULL) {
            return 1;
    }



    free(l);

    fclose(f);
    return 0;
}
int ast_add_node(AST* ast, Node* n) {
    ast->nodes[ast->nodes_count++] = n;
    if (ast->nodes_count >= ast->max_nodes) {
        ast->max_nodes *= 2;
        ast->nodes = realloc(ast->nodes, ast->max_nodes);
        if (ast->nodes == NULL) {
            fprintf(stderr, "Failed to reallocate memory for ast.\n");
            exit(1);
            return 1;
        }
    }
    return 0;
}

Node* arena_add_node(Arena* a, Node n) {
    return arena_add(a, sizeof(Node), &n);
}

#include "parse_number.c"
Node* parse_expression(Arena* a, Token* tokens, size_t* i, size_t len) {
    Node n;
    #define current tokens[*i]
    #define peek tokens[(*i) + 1]
    #define consume tokens[(*i)++]


    Token number = consume;
    if (number.type != TokenNumber) {
        fprintf(stderr, "Expected number, gor: %s.\n", get_token_type(number.type));
        return NULL;
    }
    double out;
    if (!parse_number(number.number.name, number.number.length, &out)) {
        fprintf(stderr, "Failed to parse number.\n");
        return NULL;
    }
printf("%lf\n", out);

    Node node;
    node.type = NodeNumLit;
    node.number = out;
    Node* ret_n = arena_add_node(a,node);
    printf("%lf\n", ret_n->number);


    #undef consume
    #undef peek
    #undef current
    return ret_n;
}

AST* parse(Lexer* l, Arena* a) {
    AST* ast = (AST*)malloc(sizeof(AST));
    ast->max_nodes=1024;
    ast->nodes = (Node**)malloc(sizeof(Node*)*ast->max_nodes);
    ast->nodes_count = 0;

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
        printf("\tCurrent: %s:\n", get_token_type(current.type));
        if (current.type != TokenKeyword) {
            fprintf(stderr, "Expected keyword, got: %s.\n", get_token_type(current.type));
            return NULL;
        }
        // we know it's a keyword
        // let <name> ...
        if (current.kw == KwLet) {
            consume;
            if (current.type != TokenIdent) {
                fprintf(stderr, "Expected identifier after \"let\", got: %s.\n", get_token_type(current.type));
                return NULL;
            }
            // get name
            Name identifier = consume.ident;
            printf("new var: ");
            fwrite(identifier.name, 1, identifier.length, stdout);
            printf("\n");

            // allocate and add to ast
            Node n;
            n.type = NodeVarDec;
            n.var_dec = identifier;
            ast_add_node(ast, arena_add_node(a, n));
            // if it's a semicolon then just declare eg: let i;
            if (current.type == TokenSemicolon) consume;
            else if (current.type == TokenAssign) {
                consume;
                // parse expression
                Node* n = parse_expression(a, tokens, &i, len);
                printf("Number: %lf\n", n->number);

            }
        } else {
        }

    }

    #undef consume
    #undef peek
    #undef current
    print_ast(ast);
    return ast;
}
