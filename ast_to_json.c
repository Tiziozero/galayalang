#include "ast_to_json.h"
#include "parser.h"

#define ast_to_json_test_main
#ifdef ast_to_json_test_main
int main(int argc, char** argv) {
    int status = 0;
	char* path;
    if (argc < 2) {
        // err( "Expected arguments.\nUsage: <program> <file>\n");
        // return 1;
		path = "main.gala";
    } else {
		path = argv[1];
	}
    // read file
    FILE* f = fopen(path, "rb");
    if (!f) {
        err( "Couldn't open file %s", path);
        return 1;
    }
    dbg("opened file");
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);
    dbg("got end");
    char buf[1024*1024];
    fread(buf, 1, sizeof(buf), f);
    dbg("read file");
    fclose(f); // free file
    dbg("closed file");
    buf[length] = '\0';

    Lexer* l = lexer(buf, length);
    if (!l) {
        err( "Failed to lex (?) file.");
        free(f);
        return 1;
    }

    ParserCtx* pctx = parse(l);
    if (pctx == NULL) {
            err("Failed to parse Tokens.");
            return 1;
    }

	info("Before test");
	char* ast_json = ast_to_json(pctx->ast);
	if (!ast_json) {
		err("Failed to create ast json");
		return 1;
	}
	info("after");
	printf("%s\n", ast_json);
	

    if (!pctx_destry(pctx)) {
        err("Failed to free parser context");
    }
    free(l->tokens);
    free(l);
    return status;
}
#endif
