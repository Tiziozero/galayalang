#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gala.h>

typedef enum {
    KW_IF,
    KW_WHILE,
    KW_FOR,
    KW_SWITCH,
    KW_CASE,
    KW_ELSE,
} KeyWord;


typedef enum {
    BT_INT,
    BT_CHAR,
    BT_FLOAT,
    BT_DOUBLE,
    BT_None,
} BuiltInTypes;
typedef enum {
    BE_STRUCT,
    BE_UNION,
    BE_ENUM,
} BuiltInExtra;

typedef enum {
    TOKEN_EOF,
    TOKEN_IDENT,
    TOKEN_O_BRAC,     // (
    TOKEN_C_BRAC,     // )
    TOKEN_O_CBRAC,    // {
    TOKEN_C_CBRAC,    // }
    TOKEN_O_SBRAC,    // [
    TOKEN_C_SBRAC,    // ]
    TOKEN_COMMA,      // ,
    TOKEN_KW,
    TOKEN_NUM,
    TOKEN_CHAR,
    TOKEN_STRING,
    TOKEN_PNKT, // punctuation: &, ",", *, |, \,...
    TOKEN_PREPROC,
    TOKEN_COMMENT,
    TOKEN_EQUAL,
} TokenType;

char* get_token_type_name(TokenType t) {
    switch (t) {
        case TOKEN_EOF: return "TOKEN_EOF";
        case TOKEN_IDENT: return "TOKEN_IDENT";
        case TOKEN_O_BRAC: return "TOKEN_O_BRAC";
        case TOKEN_C_BRAC: return "TOKEN_C_BRAC";
        case TOKEN_O_CBRAC: return "TOKEN_O_CBRAC";
        case TOKEN_C_CBRAC: return "TOKEN_C_CBRAC";
        case TOKEN_O_SBRAC: return "TOKEN_O_SBRAC";
        case TOKEN_C_SBRAC: return "TOKEN_C_SBRAC";
        case TOKEN_COMMA: return "TOKEN_COMMA";
        case TOKEN_KW: return "TOKEN_KW";
        case TOKEN_NUM: return "TOKEN_NUM";
        case TOKEN_CHAR: return "TOKEN_CHAR";
        case TOKEN_STRING: return "TOKEN_STRING";
        case TOKEN_PNKT: return "TOKEN_PNKT";
        case TOKEN_PREPROC: return "TOKEN_PREPROC";
        case TOKEN_COMMENT: return "TOKEN_COMMENT";
        case TOKEN_EQUAL: return "TOKEN_EQUAL";
        default: FAILED("UNKNOWN");
        }
}
typedef struct {
    TokenType type;
    union {
        struct {
            char* name;
            size_t length;
        } ident_data;
        struct {
            char* start;
            size_t length;
            float parsed_what;
        } number;
        struct {
            char* start;
            size_t length;
        } str_data;
    };
} Token;

char* kw[] = {
    "if",
    "while",
    "for",
    "return",
    "switch",
    "case",
    "static",
};
usize kwlen = sizeof(kw)/sizeof(kw[0]);
char* built_in_types[] = {
    "char",
    "long",
    "int",
    "short",
    "double",
    "float"
};
usize built_in_types_len = sizeof(built_in_types)/sizeof(built_in_types[0]);

typedef enum {
    NODE_TRANSLATION_UNIT,
    NODE_FUNC_DEF,
    NODE_DECL,
    NODE_COMPOUND_STMT,
    NODE_RETURN,
    NODE_IF,
    NODE_WHILE,
    NODE_EXPR_STMT,
    NODE_BINOP,
    NODE_UNOP,
    NODE_IDENT,
    NODE_NUM,
    NODE_CALL,
    NODE_CAST,
    // add more as needed
} NodeKind;
typedef struct Node Node;
typedef struct NodeList NodeList;
struct Node{
    NodeKind kind;
    const Token* token;
    union {
        struct {Node* left,* right; int op;} binary_op;
        struct {Node* expr; int op;} unary_op;
        struct {char* name; } identifier;
        struct {long long val; } number;
        struct {Node*func;NodeList* args;} function;
        struct { NodeList *stmts; } compound;
        struct { char *typename; char *name; Node *init; } decl;
        struct { char *ret_type; NodeList *params; Node *body; char *name; } func_dec;
    };
} ;
typedef struct {
    DynamicArray* nodes;
} AST;

#include <stdio.h>

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
    printf("Token { type = %s", token_type_to_string(t->type));

    switch (t->type) {
        case TOKEN_IDENT:
            printf(", ident = '%.*s'", 
                   (int)t->ident_data.length,
                   t->ident_data.name);
            break;

        case TOKEN_NUM:
            printf(", number = '%.*s', parsed = %f",
                   (int)t->number.length,
                   t->number.start,
                   t->number.parsed_what);
            break;

        case TOKEN_STRING:
            printf(", string = \"%.*s\"",
                   (int)t->str_data.length,
                   t->str_data.start);
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
#define peek() *(buf+i+1)
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
        if (current == '=') {
            empty_add_token(TOKEN_EQUAL);
        }
        if (current == ',') {
            empty_add_token(TOKEN_COMMA);
        }
        if (current == '/'&&peek() == '/') {
            printf("parsing single line string\n");
            do {i++;} while (current != '\n');
            i++;
            continue;
        }
        if (current == '/'&&peek() == '*') {
            printf("parsing multi line string\n");
            i+= 2;
            do {i++;} while (!(current == '*' && peek() == '/'));
            i+=2;
            continue;
        }
        if (current == '\n' || current == '\r' || current == '\t') i++;
        if (is_alpha(current)) {
            printf("identifier:\t\t");
            identifier_buffer_index = buf+i;
            do {
                i++;
            } while ( is_alpha(current) || is_numeric(current) || current == '_' );
            identifier_length = (size_t)buf + i - (size_t)identifier_buffer_index;

            loop(kwlen) {
                if (str_cmp(kw[i], identifier_buffer_index)) {
                    Info("KeyWord detected.\n");
                }

            }

            printf("\"");
            fwrite(identifier_buffer_index,1,identifier_length,stdout);
            printf("\"\n");

            Token t = {
                .type=TOKEN_IDENT,
                .ident_data={
                    .name=identifier_buffer_index,
                    .length=identifier_length,
                }
            };
            identifier_length = 0;
            tokens[count++] = t;
            continue;
        }
        if (is_numeric(current)) { // TODO: fix so that 123e-3 returns a float
            printf("parsing number: ");
            identifier_buffer_index = buf + i;
            int dot_count= 0;
            int exponent= 0;
            int parsed_negative= 0;
            do {
                if (current == '.' && !dot_count) dot_count++;
                else if (current == 'e' && !exponent) exponent ++;
                // if previous is e and haven't parsed negative exponent
                else if (current == '-' && exponent && prev=='e')  parsed_negative++;
                i++;
            } while (
            is_numeric(current) ||
            (current == 'e' && !exponent) ||
            (current == '-' && !parsed_negative)||
            (current == '.' && !dot_count) );
            identifier_length = (size_t)buf + i - (size_t)identifier_buffer_index;



            char* tmp = malloc(identifier_length*sizeof(char)+1);
            if (!tmp) {return NULL;}
            memcpy(tmp, identifier_buffer_index, identifier_length);
            tmp[identifier_length] = 0;
            println("%s:", tmp);

            Token t;
            t = (Token){
                .type=TOKEN_NUM,
                .number={
                    .parsed_what=0,
                    .start=identifier_buffer_index,
                    length=identifier_length
                },
            };
            free(tmp);
            tokens[count++] = t;
        }
        if (current == '"') {
            printf("parsing string: ");
            i++;
            char* str_start = buf+i;

            do {
                i++;
            } while (current!='"' || (current=='"'&&prev=='\\'));
            // parse string
            size_t len = buf+i - str_start;
            Token t = {
                .type=TOKEN_STRING,
                .str_data = {
                    .start = str_start,
                    .length = len,
                }
            };
            // printf("\"");
            fwrite(str_start,1,len,stdout);
            printf("\n");
            tokens[count++] = t;
        }
        if (current=='(') {
            empty_add_token(TOKEN_O_BRAC);
        }
        if (current==')') {
            empty_add_token(TOKEN_C_BRAC);
        }
        if (current=='[') {
            empty_add_token(TOKEN_O_SBRAC);
        }
        if (current==']') {
            empty_add_token(TOKEN_C_SBRAC);
        }
        if (current=='{') {
            empty_add_token(TOKEN_O_CBRAC);
        }
        if (current=='}') {
            empty_add_token(TOKEN_C_CBRAC);
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

int parser(Token* tokens, size_t length);

int main(int argc, char** argv) {
    if (argc < 2) { FAILED("must iput file"); }
    if (strlen(argv[1]) == 0) FAILED("Invalid string length??");

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

    int res ;
    if ((res = parser(tokens, token_stream_length)) != 0) FAILED("Failed to parse tokens: %d.", res);

    printf("End of parsing\n");
    free(buf);
    fclose(fp);
    return 0;
}


int expect_function(NodeList* nodes) {

    return 0;
}

char* storage_class[] = {
    "static",
    "extern",
    "typedef",
    "register",
    "auto",
};
char* modifiers[] = {
    "signed",
    "unsigned",
    "long long",
};

// [storage class] [type qualifiers] [type specifiers] declarator [= initializer];
int parse_decleration(Token* tokens, usize current, usize length) {

    return 0;
}
typedef struct {
    DynamicArray* vars;
    DynamicArray* funcs;
    DynamicArray* types;
    DynamicArray* keywords;
}Ctx;
bool parse_statement(Ctx* ctx,Token* tokens, usize size,  usize *cur, AST* ast);
bool parse_expression(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast);

typedef struct {
    char* name;
    usize length;
} Name;
int parser(Token* tokens, size_t length) {
    Ctx ctx = {};
    ctx.vars = da_new(Name);
    ctx.funcs = da_new(Name);;
    ctx.types = da_new(Name);;
    ctx.keywords = da_new(Name);;
    for (int i = 0; i < kwlen; i++) {
        add_to_da(ctx.keywords, &(Name){.name=kw[i], .length=strlen(kw[i])});
    }
    for (int i = 0; i < built_in_types_len; i++) {
        add_to_da(ctx.types, &(Name){.name=built_in_types[i], .length=strlen(built_in_types[i])});
    }
    Info("Size of types da: %lu\n", ctx.types->count);
    for (size_t i = 0; i < length; i++) {
        Token t = tokens[i];
        switch (t.type) {
            case TOKEN_EOF: println("TOKEN_EOF"); break;
            case TOKEN_IDENT:
                printf("TOKEN_IDENT\t\t");
                write_buffer_of_len(t.ident_data.name, t.ident_data.length);
                println("");
                break;
            case TOKEN_O_BRAC: println("TOKEN_O_BRAC\t\t("); break;
            case TOKEN_C_BRAC: println("TOKEN_C_BRAC\t\t)"); break;
            case TOKEN_O_CBRAC: println("TOKEN_O_CBRAC\t\t{"); break;
            case TOKEN_C_CBRAC: println("TOKEN_C_CBRAC\t\t}"); break;
            case TOKEN_O_SBRAC: println("TOKEN_O_SBRAC\t\t["); break;
            case TOKEN_C_SBRAC: println("TOKEN_C_SBRAC\t\ts]"); break;
            case TOKEN_KW: println("TOKEN_KW"); break;
            case TOKEN_CHAR: println("TOKEN_CHAR"); break;
            case TOKEN_STRING: printf("TOKEN_STRING\t\t"); write_buffer_of_len(t.str_data.start, t.str_data.length); println(""); break;
            case TOKEN_PNKT: println("TOKEN_PNKT"); break;
            case TOKEN_PREPROC: println("TOKEN_PREPROC"); break;
            case TOKEN_COMMENT: println("TOKEN_COMMENT"); break;
            case TOKEN_COMMA: println("TOKEN_COMMA"); break;
            case TOKEN_EQUAL: println("TOKEN_EQUAL"); break;
            case TOKEN_NUM: printf("FLOAT\t\t");write_buffer_of_len(t.number.start, t.number.length); println(""); break;
            default:  FAILED("Invalid token"); break;
        }
    }

    bool go = true;
    usize cur = 0;
    AST ast = {};

    ast.nodes = da_new(Node);
    while (go and cur < length) {
        go = parse_statement(&ctx,tokens, length, &cur, &ast);
    }
    return 0;
}

bool mem_cmp(char* pt1, char* pt2, usize len) {
    for (usize i = 0; i < len; i++) {
        if (pt1[i] != pt2[i]) {
            return false;
        }
    }
    return true;
}

typedef enum {
    NT_FUNC,
    NT_VAR,
    NT_TYPE,
    NT_KW,
} NameType;


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
    loop(ctx->types->count) {
        Name* current_var = ((Name*)da_get(ctx->types, i));
        if (var->length != current_var->length) continue;
        if (mem_cmp(var->name, current_var->name, var->length)) {
            *nt = NT_TYPE;
            return true;
        }
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
#define peek tokens[*cur]
bool parse_statement(Ctx* ctx,Token* tokens, usize size,  usize* cur, AST* ast) {
    Token t = consume;
    // Token t = tokens[*cur];
    // print_token(&t);
    switch (t.type) {
        case TOKEN_IDENT: {
            Name name =  {.name=t.ident_data.name, .length=t.ident_data.length};
            NameType nt;
            if (!is_in_context_decleration(ctx, &name, &nt)) {
                Info("Unknown identifier: ");
                write_buffer_of_len(name.name, name.length);
                println("");
                break;
            }
            // Info("Is in context (%d): ", nt);
            // write_buffer_of_len(name.name, name.length);
            // printf("\n");
            // print_token(&peek);
            if (peek.type == TOKEN_EOF) {
                FAILED("Bruh got EOF when identifier expexted.");
            }
            switch (nt) {
                case NT_TYPE: {
                    if (!is_type(ctx,&name)) {
                        name.name[name.length] = 0;
                        FAILED("expexted type, found %s", name.name);
                    }
                    if (peek.type != TOKEN_IDENT) {
                        print_token(&peek);
                        char* name_ = get_token_type_name(peek.type);
                        FAILED("expexted identifier, found %s", name_);
                    }
                    Token _ident = consume;
                    if (peek.type == TOKEN_EQUAL) {
                        Info("Parsing Assignment.\n");
                    }
                    break;
                }
                default: {
                    name.name[name.length] = 0;
                    FAILED("expexted something else, found %s", name.name);
                }
            }
            break;
        }
        default: break;
    };
    // *cur += 1;
    return true;
}
