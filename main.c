#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    _EOF,
    IDENT,
    O_BRAC,     // (
    C_BRAC,     // )
    O_CBRAC,    // {
    C_CBRAC,    // }
    O_SBRAC,    // [
    C_SBRAC,    // ]
    KW,
    NUM,
    FLOAT,
    CHAR,
    STR,
    PNKT, // punctuation: &, ",", *, |, \,...
    PREPROC,
    COMMENT,
} TokenType;

typedef struct {
    TokenType type;
    union {
        int number;
        struct {
            char* name;
            size_t length;
        } ident_data;
        float f;
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
#define println(msg,...) printf(msg "\n", ##__VA_ARGS__)
#define write_buffer_of_len(buffer, len) fwrite(buffer,1,len,stdout)
Token* lexer(char* buf, size_t length, size_t* size) {
    Token* tokens = malloc(10000*sizeof(Token));
    size_t count = 0;
    if (!tokens) return NULL;

    char* identifier_buffer_index = 0;
    size_t identifier_length = 0;
    
    size_t i = 0;
    while (current != '\0') {
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
            char* endptr;

            char* tmp = malloc(identifier_length*sizeof(char)+1);
            if (!tmp) {return NULL;}
            memcpy(tmp, identifier_buffer_index, identifier_length);
            tmp[identifier_length] = 0;
            printf("%s:", tmp);

            Token t;
            if (dot_count) {
                float f = strtof(tmp, &endptr);
                if (endptr == identifier_buffer_index) {
                    printf("Failed to parse number");
                    return NULL;
                }
                printf("float %f\n",f);
                t = (Token){
                    .type=FLOAT,
                    .f=f,
                };
            } else {
                int d = strtof(tmp, &endptr);
                if (endptr == identifier_buffer_index) {
                    printf("Failed to parse number");
                    return NULL;
                }
                printf("int %d\n",d);
                t = (Token){
                    .type=NUM,
                    .number=d,
                };

            }
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
    printf("\n");
    *size = count;
    return tokens;
}

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

    for (size_t i = 0; i < token_stream_length; i++) {
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
            case NUM: println("NUM"); break;
            case FLOAT: println("FLOAT"); break;
            case CHAR: println("CHAR"); break;
            case STR: printf("STR\t\t"); write_buffer_of_len(t.str_data.start, t.str_data.length); println(""); break;
            case PNKT: println("PNKT"); break;
            case PREPROC: println("PREPROC"); break;
            case COMMENT: println("COMMENT"); break;
            default:  println("Invalid token"); break;
        }
    }

    printf("End of parsing\n");
    free(buf);
    fclose(fp);
    return 0;
}
