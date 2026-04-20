all: lexer parser
	$(CC) parser.tab.c lex.yy.c -o calc -lfl
lexer: lexer.l
	flex lexer.l

parser: parser.y
	bison -d parser.y

