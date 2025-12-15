#include <alloca.h>
#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gala.h>
#include <unistd.h>
#include "token.h"
#include "node.h"
#include "map.c"
#include "parse_number.c"
#include "vm.h"



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
        default:        FAILED("UNKNOWN %d", t);
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
            empty_add_token(TOKEN_OP_PLUS);
        }else if (current == '-') {
            empty_add_token(TOKEN_OP_MINUS);
        }else if (current == '*') {
            empty_add_token(TOKEN_STAR);
        }else if (current == '/') {
            empty_add_token(TOKEN_F_SLASH);
        }else if (current == '\\') {
            empty_add_token(TOKEN_B_SLASH);
        }else if (current == '(') {
            empty_add_token(TOKEN_O_BRAC);
        }else if (current == ')') {
            empty_add_token(TOKEN_C_BRAC);
        }else if (current == '&') {
            empty_add_token(TOKEN_AMPERSAND);
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
    Arena* arena;
}Ctx;
bool parse_top_level_statement(Ctx* ctx,Token* tokens, usize size,  usize *cur, AST* ast);
Node* parse_expression(Ctx* ctx,Token* tokens, usize size,  usize* cur);
int parser(Ctx* ctx, AST* ast, Token* tokens, size_t length);
int generate_code(AST* ast, char* out_path);


void ctx_free(Ctx* ctx) {
    // free items
    free(ctx->vars_data.items);
    free(ctx->vars->items);
    free(ctx->funcs->items);
    free(ctx->keywords->items);
    free(ctx->types->items);
    // free da declarations
    free(ctx->vars);
    free(ctx->funcs);
    free(ctx->keywords);
    free(ctx->types);
}

// #define TEST_C
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
    write_buffer_of_len(buf, length);
    println("");
    Token* tokens = lexer(buf, length, &token_stream_length);
    if (!tokens) {
        printf("Faile to parse tokens.\n");
        free(buf);
        fclose(fp);
        return -1;
    }
    loop(token_stream_length) {
        print_token(&tokens[i]);
    }
    Ctx ctx;
    ctx.vars_data = kv;
    ctx.arena = &a;
    AST ast;

    int res ;
    if ((res = parser(&ctx, &ast, tokens, token_stream_length)) != 0) FAILED("Failed to parse tokens: %d.", res);


    char* out_path = "out_path.o";
    if ((res = generate_code(&ast, out_path)) != 0) FAILED("Failed to generate code: %d", res);


    printf("End of parsing\n");
    free(buf);
    fclose(fp);
    free(tokens);
    free(a.base);
    free(ast.nodes->items);
    free(ast.nodes);
    ctx_free(&ctx);
    return 0;
}
#else
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
        go = parse_top_level_statement(ctx,tokens, length, &cur, ast);
    }
    Info("Found %lu nodes...\n",ast->nodes->count);
    loop(ast->nodes->count) {
        Node* n = da_get(ast->nodes, i);
        // Info("Node: %d\n", n->kind);
        print_node(n, 0);
    }

    return 0;
}

bool is_name(Ctx* ctx, Name* var) {
    // iter names
    loop(ctx->vars->count) {
        // get name at i from da
        Name* current_var = (Name*)da_get(ctx->vars, i);
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            return true;
        }
    }
    // iter functions
    loop(ctx->funcs->count) {
        Name* current_var = (Name*)da_get(ctx->funcs, i);
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            return true;
        }
    }

    // debug, since i have to add adding names to context
    if (str_cmp("a", var->name)) return true;
    if (str_cmp("b", var->name)) return true;
    return false;
}
bool is_type(Ctx* ctx, Name* var) {
    // iter types
    loop(ctx->types->count) {
        Name* current_var = (Name*)da_get(ctx->types, i);
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
        Name* current_var = (Name*)da_get(ctx->vars, i);
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            *nt = NT_VAR;
            return true;
        }
    }
    // iter functions
    loop(ctx->funcs->count) {
        Name* current_var = (Name*)da_get(ctx->funcs, i);
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
        Name* current_var = (Name*)da_get(ctx->keywords, i);
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
#define new_node aalloc(ctx->arena, sizeof(Node));
bool parse_statement(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast);
bool type_specifier();
bool parse_top_level_statement(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast) {
    Token t = consume;
    // Token t = tokens[*cur];
    // print_token(&t);
    switch (t.type) {
        case TOKEN_SEMI: {
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
                    n.var_decleration.name = _identifier.name;
                    n.var_decleration.type = type;
                    n.var_decleration.size = 8;
                    Node* a = alloca(sizeof(Node));
                    *a = n;
                    add_to_da(ast->nodes, a);

                } else if (peek.type == TOKEN_EQUAL) {
                    Info("Assignment\n");
                    t = consume;
                    Node n;
                    Node* expr = parse_expression(ctx,tokens,size,cur);
                    if (expr == 0) FAILED("Failed to parse assignment: %lu", (usize)expr);

                    // declaration + assignment
                    n.kind = NODE_VAR_DECLRETATION;
                    n.var_decleration.name = _identifier.name;
                    n.var_decleration.type = type;
                    n.var_decleration.size = 8;
                    add_to_da(ast->nodes, &n);

                    n = (Node){};
                    n.kind = NODE_VAR_ASSIGNMENT;
                    n.assignment.target=_identifier.name;
                    n.assignment.value = expr;
                    add_to_da(ast->nodes, &n);
                    break;
                    // function declaration
                } else if (peek.type == TOKEN_O_BRAC) {

                }
            } else if (is_name(ctx, &token)) {
                Name name = token;
                if (peek.type == TOKEN_EQUAL) { // assignment
                    consume;
                    Node* expression = parse_expression(ctx, tokens,  size, cur);
                    if (!expression) {
                        FAILED("Failed to parse expression");
                        return false;
                    }
                    Node* n = new_node;
                    n->kind = NODE_VAR_ASSIGNMENT;
                    n->assignment.target = name;
                    n->assignment.value = expression;
                    add_to_da(ast->nodes, n);
                }
            }
            break;
        }
        case TOKEN_EOF: {
            return false;
        }
        default: FAILED("Unexpected %s", get_token_type_name(t.type));
    };
    return true;
}

// expression   = term { ("+" | "-") term };
// term         = factor { ("*" | "/") factor
// factor       = NUMBER <
//              | IDENT <
//              | "(" expression ")" <
//              | "*" factor <
//              | "&" factor <
//              | factor "[" expression "]"
//              | IDENT "(" [ expression { "," expression } ] ")"
//              | "-" factor
//              | "+" factor
//              | "!" factor
//              | "~" factor;
// func_call    = factor "(" [ expression { "," expression } ] ")"
bool parse_factor(Ctx* ctx,Token* tokens, usize size,  usize* cur, Node* node) {
    try(peek.type != TOKEN_EOF);
    Token t = consume;
    if ( t.type == TOKEN_NUM) { // number -> parse number
        double parsed;
        if (!parse_number(t.number.name, t.number.length,&parsed))
            FAILED("Failed to parse number.");
        Node*p = new_node; // new node of type number
        p->kind=NODE_NUM_LITERAL;
        p->num_literal.n = parsed;
        *node = *p;
        return true;
    } else if ( t.type == TOKEN_STAR) { // dereference
        Node* p = new_node;
        p->kind=NODE_VAR_DEREF;
        Node* ident = new_node;
        p->deref.deref_var = ident;
        try(parse_factor(ctx,tokens,size,cur, p->deref.deref_var));
        *node = *p;
        return true;
    } else if ( t.type == TOKEN_AMPERSAND) { // reference
        Node* p = new_node;
        p->kind=NODE_VAR_REF;
        Node* ident = new_node;
        p->reference.ref_var = ident;
        try(parse_factor(ctx,tokens,size,cur, p->reference.ref_var));
        *node = *p;
        return true;
    } else if ( t.type == TOKEN_IDENT) { // identifier
        Node*p = new_node;
        p->kind=NODE_VAR;
        p->var = t.name;
        *node = *p;
        if (peek.type == TOKEN_O_SBRAC) { // open square bracket, array access
        
        } // this is now type work, which idk really
        return true;
    } else if (t.type == TOKEN_O_BRAC)  {
        Node* right = parse_expression(ctx, tokens, size, cur);
        print_node(right, 4);
        try(right); // make sure it's not 0
        if (peek.type != TOKEN_C_BRAC) {
            FAILED("Expected close bracket found: %s", get_token_type_name(peek.type));
            return false;
        }
        consume;
        *node = *right;
        return true;
    } else {
        FAILED("Not number or ident but: %s", get_token_type_name(t.type));
        return false;
    }
    return false;
}

bool parse_term(Ctx* ctx,Token* tokens, usize size,  usize* cur, Node** node) {
    try(peek.type != TOKEN_EOF);
    Node factor;
    try(parse_factor(ctx, tokens, size, cur,&factor));

    // find * or / then expect factor
    if (peek.type == TOKEN_STAR || peek.type == TOKEN_F_SLASH) {
        // get operator we know to be * or /
        Token op = consume;

        // parse next factor
        Node* next_term;
        try(parse_term(ctx,tokens,size,cur,&next_term));

        Node* p = new_node;
        Node* left = new_node;
        Node* right = new_node;
        *left = factor;
        *right = *next_term;

        p->kind = NODE_OP;
        p->operation.op = op;
        p->operation.left = left;
        p->operation.right = right;
        *node = p;
        return true;
    } else if (
        //  operations that are valid but not handeled here/all for expression
        peek.type == TOKEN_SEMI || peek.type == TOKEN_OP_PLUS
        || peek.type == TOKEN_OP_MINUS ||
        
        // close brakcets because expression won't catch it
        peek.type == TOKEN_C_BRAC || peek.type == TOKEN_C_SBRAC)  {
        
        Node* factor_ptr = new_node;
        *factor_ptr = factor;
        *node = factor_ptr;
        return true;

    } else {
        FAILED("Expected semicolon, found: %s", get_token_type_name(peek.type));
        return false;
    } 
}
Node* parse_expression(Ctx* ctx,Token* tokens, usize size,  usize* cur) {
    // see if it's end of file/end of token stream
    try(peek.type != TOKEN_EOF);

    // parse term
    Node* term = 0;
    try(parse_term(ctx,tokens,size,cur,&term));

    // then, if next token is "+" or "-" parse anothe expression
    // not term because term doesn't consider another "+" or "-"
    if (peek.type == TOKEN_OP_PLUS || peek.type == TOKEN_OP_MINUS) {
        Token op = consume;
        Node* next_exp = parse_expression(ctx, tokens, size, cur);
        try(next_exp); // see if it exists
        // allocate new node in arena
        Node* p = new_node;
        p->kind = NODE_OP;
        p->operation.op = op;
        p->operation.left = term;
        p->operation.right = next_exp;
        return p;
    } else {
        return term;
    }
    return false;
}
#include "vm.h"


typedef struct {
    Name name;
    size_t size;
    size_t stack_offset;
} CGVarOffset;
typedef struct {
    Name name;
    size_t size;
    size_t stack_offset;
} CGVariables;
typedef struct {
    CGVarOffset* offsets;
    CGVariables* variables;
    size_t offset_size;
    char prog[1000];
    size_t prog_size;
} CodeGenCtx;
int get_var(CodeGenCtx* ctx,Name name) {
    for (size_t i = 0; i;)
    return 0;
}
int add_var(CodeGenCtx* ctx,Name name) {
    ctx->offsets[ctx->offset_size].name = name;
    ctx->offsets[ctx->offset_size].stack_offset = ctx->offset_size;
    return 0;
}
int code_gen_node(CodeGenCtx* ctx, Node* node) {
    switch (node->kind) {
        case NODE_VAR_DECLRETATION: {
            Info("assignment\n");
            // push to stack?
            try( add_var(ctx,node->var_decleration.name) );
            ctx->offset_size+=node->var_decleration.size;

            ctx->prog[ctx->prog_size++] = make_instruction(VM_OP_PUSH, 0);
            ctx->prog[ctx->prog_size++] = R1;


            break;
        }
        case NODE_VAR_ASSIGNMENT: {


        }
        default: {
            FAILED("Invalide operation: %d", node->kind);
            return 0;
        }
    }
    return 1;
}


int generate_code(AST* ast, char* out_path) {
    cpu8_t* cpu = cpu8_create();
    // Arena a = arena_create(1024*1024);

    CGVarOffset offsets[1024];

    CodeGenCtx ctx;
    ctx.offsets = offsets;
    ctx.offset_size = 0;

    for (size_t stmt_index = 0; stmt_index < ast->nodes->count; stmt_index++) {
        try(code_gen_node(&ctx, da_get(ast->nodes, stmt_index)));
    }


    Info("Prorgam returned: %d\n", run(ctx.prog, ctx.prog_size));
    cpu8_destroy(cpu);
    return 0;
    FILE* f = fopen(out_path, "wb");
    if (!f) FAILED("Failed to open file: %s", out_path);
    fwrite("Hello, World!", 1, 16, f);
    fclose(f);

    return 0;
}
