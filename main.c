#include <stddef.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gala.h>

typedef enum {
    _EOF,
    IDENT,
    O_BRAC,     // (
    C_BRAC,     // )
    O_CBRAC,    // {
    C_CBRAC,    // }
    O_SBRAC,    // [
    C_SBRAC,    // ]
    COMMA,      // ,
    KW,
    NUM,
    CHAR,
    STR,
    PNKT, // punctuation: &, ",", *, |, \,...
    PREPROC,
    COMMENT,
} TokenType;



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
#define current buf[i]
#define prev buf[i-1]
#define peek() *(buf+i+1)
#define is_alpha(c) ((c>='a'&&c<='z')||(c>='A'&&c<='Z'))
#define is_numeric(c) ((c>='0'&&c<='9'))
#define print_i printf("%lu\n",i)
#define add_token(t) tokens[count++] = t
#define empty_add_token(t) tokens[count++] = (Token){.type=t}
#define println(start,...) printf(start "\n", ##__VA_ARGS__)
#define write_buffer_of_len(buffer, len) fwrite(buffer,1,len,stdout)
Token* lexer(char* buf, size_t length, size_t* size) {
    Token* tokens = malloc(10000*sizeof(Token));
    size_t count = 0;
    if (!tokens) return NULL;

    char* identifier_buffer_index = 0;
    size_t identifier_length = 0;
    
    size_t i = 0;
    while (current != '\0') {
        if (current == ',') {
            empty_add_token(COMMA);
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
            printf("\"");
            fwrite(identifier_buffer_index,1,identifier_length,stdout);
            printf("\"\n");
            Token t = {
                .type=IDENT,
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
                .type=NUM,
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
                .type=STR,
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
            empty_add_token(O_BRAC);
        }
        if (current==')') {
            empty_add_token(C_BRAC);
        }
        if (current=='[') {
            empty_add_token(O_SBRAC);
        }
        if (current==']') {
            empty_add_token(C_SBRAC);
        }
        if (current=='{') {
            empty_add_token(O_CBRAC);
        }
        if (current=='}') {
            empty_add_token(C_CBRAC);
        }
        i++;
    }
    empty_add_token(_EOF);
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

int main(void) {
    FILE* fp = fopen("test.c", "rb");
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
struct NodeList {
    Node *node;
    Node *start;
    NodeList *next;
};

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

int parser(Token* tokens, size_t length) {
    for (size_t i = 0; i < length; i++) {
        Token t = tokens[i];
        switch (t.type) {
            case _EOF: println("_EOF"); break;
            case IDENT:
                printf("IDENT\t\t");
                write_buffer_of_len(t.ident_data.name, t.ident_data.length);
                println("");
                break;
            case O_BRAC: println("O_BRAC\t\t("); break;
            case C_BRAC: println("C_BRAC\t\t)"); break;
            case O_CBRAC: println("O_CBRAC\t\t{"); break;
            case C_CBRAC: println("C_CBRAC\t\t}"); break;
            case O_SBRAC: println("O_SBRAC\t\t["); break;
            case C_SBRAC: println("C_SBRAC\t\ts]"); break;
            case KW: println("KW"); break;
            case CHAR: println("CHAR"); break;
            case STR: printf("STR\t\t"); write_buffer_of_len(t.str_data.start, t.str_data.length); println(""); break;
            case PNKT: println("PNKT"); break;
            case PREPROC: println("PREPROC"); break;
            case COMMENT: println("COMMENT"); break;
            case COMMA: println("COMMA"); break;
            case NUM: printf("FLOAT\t\t");write_buffer_of_len(t.number.start, t.number.length); println(""); break;
            default:  FAILED("Invalid token"); break;
        }
    }

    bool go = true;
    usize cur = 0;
    #define c tokens[cur]
    #define type c.type


    DynamicArray* nodes = da_new(Node);


    while (go and cur < length) {
        switch (type) {
            case IDENT: {
                loop(kwlen) {
                    if (str_cmp(kw[i], c.ident_data.name)) {
                        // handle_kw(tokens, cur, kw[i]);
                    }
                }
                break;
            }
            default: FAILED("Invalid tokens: %d", type);
        }
        cur++;
    }
    

    return 0;
}
