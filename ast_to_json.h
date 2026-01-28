#ifndef AST_TO_JSON_H
#define AST_TO_JSON_H

#include "parser.h"
#include <stdio.h>
#include <string.h>

typedef struct {
    char* buffer;
    size_t size;
    size_t capacity;
} JsonBuilder;

static inline JsonBuilder json_builder_new(size_t initial_capacity) {
    JsonBuilder jb;
    jb.capacity = initial_capacity;
    jb.size = 0;
    jb.buffer = (char*)malloc(jb.capacity);
    if (jb.buffer) {
        jb.buffer[0] = '\0';
    }
    return jb;
}

static inline void json_builder_append(JsonBuilder* jb, const char* str) {
    size_t len = strlen(str);
    while (jb->size + len + 1 > jb->capacity) {
        jb->capacity *= 2;
        jb->buffer = (char*)realloc(jb->buffer, jb->capacity);
    }
    strcpy(jb->buffer + jb->size, str);
    jb->size += len;
}

static inline void json_builder_appendf(JsonBuilder* jb, const char* fmt, ...) {
    char temp[1024];
    va_list args;
    va_start(args, fmt);
    vsnprintf(temp, sizeof(temp), fmt, args);
    va_end(args);
    json_builder_append(jb, temp);
}

static inline void json_escape_string(JsonBuilder* jb, const char* str, size_t len) {
    json_builder_append(jb, "\"");
    for (size_t i = 0; i < len; i++) {
        char c = str[i];
        switch (c) {
            case '"':  json_builder_append(jb, "\\\""); break;
            case '\\': json_builder_append(jb, "\\\\"); break;
            case '\n': json_builder_append(jb, "\\n"); break;
            case '\r': json_builder_append(jb, "\\r"); break;
            case '\t': json_builder_append(jb, "\\t"); break;
            default:
                if (c >= 32 && c <= 126) {
                    char buf[2] = {c, '\0'};
                    json_builder_append(jb, buf);
                } else {
                    json_builder_appendf(jb, "\\u%04x", (unsigned char)c);
                }
        }
    }
    json_builder_append(jb, "\"");
}

static inline void type_to_json(JsonBuilder* jb, const Type* type) {
    if (!type) {
        json_builder_append(jb, "null");
        return;
    }
    
    json_builder_append(jb, "{");
    json_builder_appendf(jb, "\"type\":%d,", type->type);
    json_builder_appendf(jb, "\"size\":%zu,", type->size);
    json_builder_append(jb, "\"name\":");
    json_escape_string(jb, type->name.name, type->name.length);
    
    if (type->type == tt_ptr) {
        json_builder_append(jb, ",\"ptr\":");
        type_to_json(jb, type->ptr);
    } else if (type->type == tt_array) {
        json_builder_append(jb, ",\"array\":{");
        json_builder_append(jb, "\"element_type\":");
        type_to_json(jb, type->array.type);
        json_builder_append(jb, "}");
    }
    
    json_builder_append(jb, "}");
}

static void node_to_json(JsonBuilder* jb, const Node* node);

static inline void node_array_to_json(JsonBuilder* jb, Node** nodes, size_t count) {
    json_builder_append(jb, "[");
    for (size_t i = 0; i < count; i++) {
        if (i > 0) json_builder_append(jb, ",");
        node_to_json(jb, nodes[i]);
    }
    json_builder_append(jb, "]");
}

static void node_to_json(JsonBuilder* jb, const Node* node) {
    if (!node) {
        json_builder_append(jb, "null");
        return;
    }
	info("Node %s", node_type_to_string(node->type));
    
    json_builder_append(jb, "{");
    json_builder_appendf(jb, "\"node_type\":\"%s\"", node_type_to_string(node->type));
    /* json_builder_append(jb, "\"expr_type\":");
    type_to_json(jb, &node->expr_type); */
    
	info("switch type");
    switch (node->type) {
        case NodeNone:
            break;
            
        case NodeCast:
            json_builder_append(jb, ",\"to\":");
            node_to_json(jb, node->cast.to);
            json_builder_append(jb, ",\"expr\":");
            node_to_json(jb, node->cast.expr);
            break;
            
        case NodeVarDec:
            json_builder_append(jb, ",\"name\":");
            json_escape_string(jb, node->var_dec.name.name, node->var_dec.name.length);
            json_builder_append(jb, ",\"type\":");
            type_to_json(jb, &node->var_dec.type);
            json_builder_append(jb, ",\"value\":");
            node_to_json(jb, node->var_dec.value);
            break;
            
        case NodeVar:
            json_builder_append(jb, ",\"name\":");
            json_escape_string(jb, node->var.name.name, node->var.name.length);
            break;
            
        case NodeNumLit:
            json_builder_appendf(jb, ",\"value\":%g", node->number.number);
            json_builder_append(jb, ",\"str_repr\":");
            json_escape_string(jb, node->number.str_repr.name, node->number.str_repr.length);
            break;
            
        case NodeArg:
            json_builder_append(jb, ",\"name\":");
            json_escape_string(jb, node->arg.name.name, node->arg.name.length);
            json_builder_append(jb, ",\"type\":");
            node_to_json(jb, node->arg.type);
            break;
            
        case NodeUnary:
            json_builder_appendf(jb, ",\"unary_type\":\"%s\",", unary_type_to_string(node->unary.type));
            json_builder_append(jb, "\"target\":");
            node_to_json(jb, node->unary.target);
            break;
            
        case NodeBinOp:
            json_builder_appendf(jb, ",\"op\":\"%s\",", optype_to_string(node->binop.type));
            json_builder_append(jb, "\"left\":");
            node_to_json(jb, node->binop.left);
            json_builder_append(jb, ",\"right\":");
            node_to_json(jb, node->binop.right);
            break;
            
        case NodeFnDec:
			info("fn dec");
            json_builder_append(jb, ",\"name\":");
            json_escape_string(jb, node->fn_dec.name.name,
					node->fn_dec.name.length);
			info("fn args");
            json_builder_append(jb, ",\"args\":");
            node_array_to_json(jb, node->fn_dec.args,
					node->fn_dec.args_count);
			info("fn return rype");
            json_builder_append(jb, ",\"return_type\":");
            json_builder_append(jb, "\"todo\"");
            // node_to_json(jb, node->fn_dec.return_type);
            json_builder_append(jb, ",\"body\":");
            node_to_json(jb, node->fn_dec.body);
            break;
            
        case NodeIfElse:
            json_builder_append(jb, ",\"base_condition\":");
            node_to_json(jb, node->if_else_con.base_condition);
            json_builder_append(jb, ",\"base_block\":");
            node_to_json(jb, node->if_else_con.base_block);
            json_builder_append(jb, ",\"alternate_conditions\":");
            node_array_to_json(jb, node->if_else_con.alternate_conditions, node->if_else_con.count);
            json_builder_append(jb, ",\"alternate_blocks\":");
            node_array_to_json(jb, node->if_else_con.alternate_blocks, node->if_else_con.count);
            json_builder_append(jb, ",\"else_block\":");
            node_to_json(jb, node->if_else_con.else_block);
            break;
            
        case NodeFnCall:
            json_builder_append(jb, ",\"fn_name\":");
            json_escape_string(jb, node->fn_call.fn_name.name, node->fn_call.fn_name.length);
            json_builder_append(jb, ",\"args\":");
            node_array_to_json(jb, node->fn_call.args, node->fn_call.args_count);
            break;
            
        case NodeConditional:
            json_builder_append(jb, ",\"condition\":");
            node_to_json(jb, node->conditional.condition);
            json_builder_append(jb, ",\"if_true\":");
            node_to_json(jb, node->conditional.left_true);
            json_builder_append(jb, ",\"if_false\":");
            node_to_json(jb, node->conditional.right_false);
            break;
            
        case NodeBlock:
            json_builder_appendf(jb, ",\"nodes_count\":%zu,", node->block.nodes_count);
            json_builder_append(jb, "\"nodes\":");
            node_array_to_json(jb, node->block.nodes, node->block.nodes_count);
            break;
            
        case NodeRet:
            json_builder_append(jb, ",\"value\":");
            node_to_json(jb, node->ret);
            break;
            
        case NodeTypeData:
            json_builder_append(jb, ",\"type_data\":");
            type_to_json(jb, &node->type_data);
            break;
            
        default:
            break;
    }
    
    json_builder_append(jb, "}");
}

static inline char* ast_to_json(const AST* ast) {
    if (!ast) {
        return NULL;
    }
    
    JsonBuilder jb = json_builder_new(4096);
    
    json_builder_append(&jb, "{");
    json_builder_appendf(&jb, "\"nodes_count\":%zu,", ast->nodes_count);
    json_builder_appendf(&jb, "\"max_nodes\":%zu,", ast->max_nodes);
    json_builder_append(&jb, "\"nodes\":[");
    
    for (size_t i = 0; i < ast->nodes_count; i++) {
		info("loop");
        if (i > 0) json_builder_append(&jb, ",");
        node_to_json(&jb, ast->nodes[i]);
    }
    
    json_builder_append(&jb, "]}");
    
    return jb.buffer;
}

static inline void ast_to_json_file(const AST* ast, const char* filename) {
    char* json = ast_to_json(ast);
    if (!json) {
        err("Failed to convert AST to JSON");
        return;
    }
    
    FILE* f = fopen(filename, "w");
    if (!f) {
        err("Failed to open file: %s", filename);
        free(json);
        return;
    }
    
    fprintf(f, "%s", json);
    fclose(f);
    free(json);
    
    info("AST written to %s", filename);
}

#endif // AST_TO_JSON_H
