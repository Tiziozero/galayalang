#ifndef PARSER_H
#define PARSER_H
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "lexer.h"
#include "utils.h"

typedef struct ProgramState ProgramState;
typedef struct Node Node;
typedef struct Parser Parser;
typedef struct Symbol Symbol;


Node* parse(Parser *p);
int parser_destry(Parser *p);
Span get_name_from_path(const char *path);


#endif // PARSER_H
