#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    _EOF,
    IDENT,
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
    };
} Token;
#define current buf[i]
#define prev buf[i-1]
#define peek() *(buf+i+1)
#define is_alpha(c) ((c>='a'&&c<='z')||(c>='A'&&c<='Z'))
#define is_numeric(c) ((c>='0'&&c<='9'))
#define print_i printf("%lu\n",i)
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
            (current == '.' && !dot_count)
        );
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
        if (peek() == '"') {
            // parse string
            
        }
        i++;
    }
    printf("\n");
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

    printf("End of parsing\n");
    free(buf);
    fclose(fp);
    return 0;
}
