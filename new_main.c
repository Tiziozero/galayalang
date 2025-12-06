#include <alloca.h>
#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gala.h>
#include "token.h"
#include "node.h"
#include "map.c"
#include "parse_number.c"



typedef enum {
    NT_FUNC,
    NT_VAR,
    NT_TYPE,
    NT_KW,
} NameType;

typedef struct {
    DynamicArray* nodes;
} AST;

static const char* token_type_to_string(TokenType t) {
    switch (t) {
        case TOKEN_EOF:      return "EOF";
        case TOKEN_IDENT:     return "TOKEN_IDENT";
        case TOKEN_O_BRAC:    return "TOKEN_O_BRAC (";
        case TOKEN_C_BRAC:    return "TOKEN_C_BRAC )";
        case TOKEN_O_CBRAC:   return "TOKEN_O_CBRAC {";
        case TOKEN_C_CBRAC:   return "TOKEN_C_CBRAC }";
        case TOKEN_O_SBRAC:   return "TOKEN_O_SBRAC [";
        case TOKEN_C_SBRAC:   return "TOKEN_C_SBRAC ]";
        case TOKEN_COMMA:     return "TOKEN_COMMA";
        case TOKEN_KW:        return "KEYWORD";
        case TOKEN_NUM:       return "NUMBER";
        case TOKEN_CHAR:      return "TOKEN_CHAR";
        case TOKEN_STRING:       return "STRING";
        case TOKEN_PNKT:      return "PUNCTUATION";
        case TOKEN_PREPROC:   return "PREPROCESSOR";
        case TOKEN_COMMENT:   return "TOKEN_COMMENT";
        case TOKEN_EQUAL:   return "TOKEN_EQUAL";
        default:        FAILED( "UNKNOWN");
    }
}

void print_token(const Token* t) {
    printf("Token { type = %s", get_token_type_name(t->type));

    switch (t->type) {
        case TOKEN_IDENT:
            printf(", ident = '%.*s'", 
                   (int)t->name.length,
                   t->name.name);
            break;

        case TOKEN_NUM:
            printf(", number = '%.*s', parsed = %f",
                   (int)t->name.length,
                   t->name.name,
                   0.0f);
            break;

        case TOKEN_STRING:
            printf(", string = \"%.*s\"",
                   (int)t->name.length,
                   t->name.name);
            break;

        default:
            // Tokens like punctuation/brackets don't need extra data
            break;
    }

    printf(" }\n");
}


/*
    true if the same, else false (1/0)
 */
bool str_cmp(char* str1, char* str2) {
    usize i = 0;
    while (str1[i] != 0 and str2[i] != 0) {
        if (str1[i] != str2[i]) return false;
        i++;
    }
    return true;
}

#define current buf[i]
#define prev buf[i-1]
#define peek buf[i+1]
#define is_alpha(c) ((c>='a'&&c<='z')||(c>='A'&&c<='Z'))
#define is_numeric(c) ((c>='0'&&c<='9'))
#define print_i printf("%lu\n",i)
#define add_token(t) tokens[count++] = t
#define empty_add_token(t) tokens[count++] = (Token){.type=t}
// #define println(start,...) printf(start "\n", ##__VA_ARGS__)
#define write_buffer_of_len(buffer, len) fwrite(buffer,1,len,stdout)
Token* lexer(char* buf, size_t length, size_t* size) {
    Token* tokens = malloc(10000*sizeof(Token));
    size_t count = 0;
    if (!tokens) return NULL;

    char* identifier_buffer_index = 0;
    size_t identifier_length = 0;
    
    size_t i = 0;
    while (current != '\0') {
        if (current == ';') {
            empty_add_token(TOKEN_SEMI);
        }else if (current == '=') {
            empty_add_token(TOKEN_EQUAL);
        }else if (current == '+') {
            Token t;
            t.type = TOKEN_OP_PLUS;
            add_token(t);
        }else if (current == '+') {
            Token t;
            t.type = TOKEN_OP_MINUS;
            add_token(t);
        } else if (is_alpha(current) || current == '_') {
            identifier_buffer_index = &current;
            identifier_length = 0;
            do { i++; identifier_length++; } while (
                is_alpha(current) || is_numeric(current) || current == '_');
            write_buffer_of_len(identifier_buffer_index, identifier_length);
            println("");
            Token t;
            t.type = TOKEN_IDENT;
            t.name.name = identifier_buffer_index;
            t.name.length = identifier_length;
            add_token(t);
            printf("Adding Token: ");
            print_token(&t);
            println("%c", current);
            i--; // go back since i increases at end of loop
        } else if (is_numeric(current)) {
            identifier_buffer_index = &current;
            identifier_length = 0;
            do { i++; identifier_length++; } while ( is_numeric(current));
            i--;
            write_buffer_of_len(identifier_buffer_index, identifier_length);
            println("");
            Token t;
            t.type = TOKEN_NUM;
            t.number.name = identifier_buffer_index;
            t.number.length = identifier_length;
            t.name.name = identifier_buffer_index;
            t.name.length = identifier_length;
            add_token(t);
            printf("Adding Token: ");
            print_token(&t);
        }

        i++;
    }
    empty_add_token(TOKEN_EOF);
    printf("\n");
    *size = count;
    return tokens;
}
#undef current
#undef current
#undef prev
#undef peek
#undef is_alpha
#undef is_numeric
#undef print_i
#undef add_token
#undef empty_add_token

typedef struct {
    DynamicArray* vars;
    DynamicArray* funcs;
    DynamicArray* types;
    DynamicArray* keywords;
    KVStore vars_data;
}Ctx;
bool parse_statement(Ctx* ctx,Token* tokens, usize size,  usize *cur, AST* ast);
bool parse_expression(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast);
int parser(Ctx* ctx, AST* ast, Token* tokens, size_t length);
int generate_code(AST* ast, char* out_path);

#define TEST_C
#ifndef TEST_C
int main(int argc, char** argv) {
    if (argc < 2) { FAILED("must iput file"); }
    if (strlen(argv[1]) == 0) FAILED("Invalid string length??");
    Arena a = arena_create(1024 * 1024); // 1MB arena
    KVStore kv = kv_create(&a, 4);

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        printf("Failed to open file\n");
        return -1;
    }
    fseek(fp, 0, SEEK_END);
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* buf = malloc(length);
    if (!buf){
        printf("Failed to create buffer.\n");
        fclose(fp);
        return -1;
    }
    memset(buf, 0, length);
    fread(buf, 1,length, fp);
    size_t token_stream_length;
    printf("Parsing fiile of size: %lu\n", length);
    Token* tokens = lexer(buf, length, &token_stream_length);
    if (!tokens) {
        printf("Faile to parse tokens.\n");
        free(buf);
        fclose(fp);
        return -1;
    }
    Ctx ctx;
    ctx.vars_data = kv;
    AST ast;

    int res ;
    if ((res = parser(&ctx, &ast, tokens, token_stream_length)) != 0) FAILED("Failed to parse tokens: %d.", res);

    char* out_path = "out_path.o";
    if ((res = generate_code(&ast, out_path)) != 0) FAILED("Failed to generate code: %d", res);


    printf("End of parsing\n");
    free(buf);
    fclose(fp);
    free(a.base);
    return 0;
}
#else
int main(int argc, char** argv) {
    char* _expression = "1 + 2";
    usize size = 0;
    Token* tokens = lexer(_expression, strlen(_expression), &size);
    if (size == 0) FAILED("Failed to parse expression??");
    Ctx ctx;
    ctx.vars = da_new(Node);
    ctx.funcs = da_new(Node);
    ctx.types = da_new(Node);
    ctx.keywords = da_new(Node);
    usize cur = 0;
    AST ast;

    Info("%d\n", parse_expression(&ctx, tokens, size, &cur, &ast));
}
#endif

int parser(Ctx* ctx, AST* ast, Token* tokens, size_t length) {
    ctx->vars = da_new(Name);
    ctx->funcs = da_new(Name);;
    ctx->types = da_new(Name);;
    ctx->keywords = da_new(Name);;
    Info("Size of types da: %lu\n", ctx->types->count);

    bool go = true;
    usize cur = 0;
    

    ast->nodes = da_new(Node);
    while (go and cur < length) {
        go = parse_statement(ctx,tokens, length, &cur, ast);
    }
    Info("Found %lu nodes...\n",ast->nodes->count);
    loop(ast->nodes->count) {
        Node* n = da_get(ast->nodes, i);
        Info("Node: %d\n", n->kind);
    }

    return 0;
}


bool is_type(Ctx* ctx, Name* var) {
    // iter types
    loop(ctx->types->count) {
        Name* current_var = ((Name*)da_get(ctx->types, i));
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            // *nt = NT_TYPE;
            return true;
        }
    }
    if (str_cmp("int", var->name)) return true;
    if (str_cmp("char", var->name)) return true;
    if (str_cmp("float", var->name)) return true;
    if (str_cmp("double", var->name)) return true;
    if (str_cmp("short", var->name)) return true;
    return false;
}
bool is_in_context_decleration(Ctx* ctx, Name* var, NameType* nt) {
    // iter variable names
    loop(ctx->vars->count) {
        Name* current_var = ((Name*)da_get(ctx->vars, i));
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            *nt = NT_VAR;
            return true;
        }
    }
    // iter functions
    loop(ctx->funcs->count) {
        Name* current_var = ((Name*)da_get(ctx->funcs, i));
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            *nt = NT_FUNC;
            return true;
        }
    }
    // iter types
    if (is_type(ctx, var)) {
        *nt = NT_TYPE;
        return true;
    }
    // iter keywords
    loop(ctx->keywords->count) {
        Name* current_var = ((Name*)da_get(ctx->keywords, i));
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            *nt = NT_KW;
            return true;
        }
    }
    
    return false;
}

// statement  = function declaration | variable declaration | assignment | ... ";"
// function declaration = type name "(" [arg]* ")" ";"
// variable declaration = type name;
// assignment = identifier "=" expression;

#define consume tokens[(*cur)++]
#define peek tokens[*(cur)]
bool parse_statement(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast) {
    Token t = consume;
    // Token t = tokens[*cur];
    // print_token(&t);
    switch (t.type) {
        case TOKEN_SEMI: {
            Info("Semi.\n");
            break;
        }
        case TOKEN_IDENT: {
            Name token = t.name;
            // if it's a type then parse variable or function
            if (is_type(ctx, &token)) {
                Name type = token;
                // if it's not a identifier that somethings wrong
                if (peek.type != TOKEN_IDENT) {
                    FAILED("Expected identifier, got: %s", token_type_to_string(peek.type));
                }
                // identifier
                Token _identifier = consume;
                if (peek.type == TOKEN_SEMI) {
                    Node n;
                    n.kind = NODE_VAR_DECLRETATION;
                    n.var_recleration.name = _identifier.name;
                    n.var_recleration.type = type;
                    Info("Added Token: NODE_VAR_DECLRETATION\n");
                    void* a = alloca(sizeof(Node));
                    add_to_da(ast->nodes, &n);
                } else if (peek.type == TOKEN_EQUAL) {
                    Info("Found assignment\n");
                    t = consume;
                    int res;
                    if ((res = parse_expression(ctx,tokens,size,cur,ast)) != 0) FAILED("Failed to parse assignment: %d", res);
                    // declaration + assignment
                    Node n;
                    n.kind = NODE_VAR_DECLRETATION;
                    n.var_recleration.name = _identifier.name;
                    n.var_recleration.type = type;
                    Info("Added Token: NODE_VAR_DECLRETATION\n");
                    void* a = alloca(sizeof(Node));
                    add_to_da(ast->nodes, &n);
                    break;
                }
            }
            break;
        }
        case TOKEN_EOF: {
            return false;
        }
        default: break;
    };
    return true;
}

// expression = term { ("+" | "-") term };
// term       = factor { ("*" | "/") factor };
// factor     = NUMBER | IDENT | "(" expression ")";
// start with number
bool parse_factor(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast) {
    Token t = consume;
    if(t.type == TOKEN_NUM) {

    }
    return false;
}
bool parse_term(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast) {
    Token t = consume;
    double parsed;
    Info("%lu, %lu\n", (usize)t.number.name, t.number.length);
    if (!parse_number(t.number.name, t.number.length,&parsed)) FAILED("Failed to parse number.");

    Info("Parsed number: %lf\n", parsed);
    return false;
}
bool parse_expression(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast) {
    parse_term(ctx,tokens,size,cur,ast);


    return false;
}
int generate_code(AST* ast, char* out_path) {
    FILE* f = fopen(out_path, "wb");
    if (!f) FAILED("Failed to open file: %s", out_path);
    

    return 0;
}
