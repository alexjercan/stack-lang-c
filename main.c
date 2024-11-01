#define DS_IO_IMPLEMENTATION
#define DS_AP_IMPLEMENTATION
#define DS_SS_IMPLEMENTATION
#define DS_SB_IMPLEMENTATION
#define DS_DA_IMPLEMENTATION
#include "ds.h"

#define SYMBOL_COMMA ','
#define SYMBOL_MINUS '-'
#define SYMBOL_GT '>'
#define SYMBOL_LPAREN '('
#define SYMBOL_RPAREN ')'

#define isname(c) ((isalnum((c)) || ispunct((c))) && (c) != SYMBOL_LPAREN && (c) != SYMBOL_RPAREN && (c) != SYMBOL_COMMA)

#define SLICE_FUNC (ds_string_slice){.str = "func", .len = 4, .allocator = NULL }
#define SLICE_IN (ds_string_slice){.str = "in", .len = 2, .allocator = NULL }
#define SLICE_END (ds_string_slice){.str = "end", .len = 3, .allocator = NULL }
#define SLICE_DATA (ds_string_slice){.str = "data", .len = 4, .allocator = NULL }

typedef enum {
    STACK_TOKEN_DATA,
    STACK_TOKEN_NAME,
    STACK_TOKEN_LPAREN,
    STACK_TOKEN_RPAREN,
    STACK_TOKEN_COMMA,
    STACK_TOKEN_FUNC,
    STACK_TOKEN_IN,
    STACK_TOKEN_END,
    STACK_TOKEN_NUMBER,
    STACK_TOKEN_EOF,
    STACK_TOKEN_ILLEGAL,
} stack_token_kind;

static const char* stack_token_kind_to_string(stack_token_kind kind) {
    switch (kind) {
    case STACK_TOKEN_DATA: return "DATA";
    case STACK_TOKEN_NAME: return "NAME";
    case STACK_TOKEN_LPAREN: return "(";
    case STACK_TOKEN_RPAREN: return ")";
    case STACK_TOKEN_COMMA: return ",";
    case STACK_TOKEN_FUNC: return "FUNC";
    case STACK_TOKEN_IN: return "IN";
    case STACK_TOKEN_END: return "END";
    case STACK_TOKEN_NUMBER: return "NUMBER";
    case STACK_TOKEN_EOF: return "<EOF>";
    case STACK_TOKEN_ILLEGAL: return "ILLEGAL";
    }
}

typedef struct {
    stack_token_kind kind;
    ds_string_slice value;
    unsigned int pos;
} stack_token;

static char* stack_token_to_string(stack_token token) {
    char *buffer = NULL;
    ds_string_builder sb = {0};
    ds_string_builder_init(&sb);

    if (ds_string_builder_append(&sb, "%s", stack_token_kind_to_string(token.kind)) != 0) {
        DS_PANIC("Failed to format token");
    }

    if (token.value.str != NULL) {
        char *buffer = NULL;

        if (ds_string_slice_to_owned(&token.value, &buffer) != 0) {
            DS_PANIC("Failed to create a string");
        }

        if (ds_string_builder_append(&sb, "(%s)", buffer) != 0) {
            DS_PANIC("Failed to format token");
        }

        DS_FREE(NULL, buffer);
    }

    if (ds_string_builder_build(&sb, &buffer) != 0) {
        DS_PANIC("Failed to build string");
    }

    ds_string_builder_free(&sb);
    return buffer;
}

typedef struct {
    const char *buffer;
    unsigned int buffer_len;
    unsigned int pos;
    unsigned int read_pos;
    char ch;
} stack_lexer;

static char stack_lexer_peek(stack_lexer *lexer) {
    if (lexer->read_pos >= lexer->buffer_len) {
        return EOF;
    }

    return lexer->buffer[lexer->read_pos];
}

static char stack_lexer_read(stack_lexer *lexer) {
    lexer->ch = stack_lexer_peek(lexer);

    lexer->pos = lexer->read_pos;
    lexer->read_pos += 1;

    return lexer->ch;
}

static void stack_lexer_skip_whitespace(stack_lexer *lexer) {
    while (isspace(lexer->ch)) {
        stack_lexer_read(lexer);
    }
}

int stack_lexer_init(stack_lexer *lexer, const char *buffer, unsigned int buffer_len) {
    lexer->buffer = buffer;
    lexer->buffer_len = buffer_len;
    lexer->pos = 0;
    lexer->read_pos = 0;
    lexer->ch = 0;

    stack_lexer_read(lexer);

    return 0;
}

stack_token stack_lexer_next(stack_lexer *lexer) {
    stack_lexer_skip_whitespace(lexer);

    unsigned int position = lexer->pos;
    if (lexer->ch == EOF) {
        stack_lexer_read(lexer);
        return (stack_token){.kind = STACK_TOKEN_EOF, .value = {0}, .pos = position };
    } else if (lexer->ch == SYMBOL_LPAREN) {
        stack_lexer_read(lexer);
        return (stack_token){.kind = STACK_TOKEN_LPAREN, .value = {0}, .pos = position };
    } else if (lexer->ch == SYMBOL_RPAREN) {
        stack_lexer_read(lexer);
        return (stack_token){.kind = STACK_TOKEN_RPAREN, .value = {0}, .pos = position };
    } else if (lexer->ch == SYMBOL_COMMA) {
        stack_lexer_read(lexer);
        return (stack_token){.kind = STACK_TOKEN_COMMA, .value = {0}, .pos = position };
    } else if (isdigit(lexer->ch) || lexer->ch == SYMBOL_MINUS) {
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 0 };

        if (lexer->ch == SYMBOL_MINUS) {
            slice.len += 1;
            stack_lexer_read(lexer);
        }

        while (isdigit(lexer->ch)) {
            slice.len += 1;
            stack_lexer_read(lexer);
        }

        return (stack_token){.kind = STACK_TOKEN_NUMBER, .value = slice, .pos = position };
    } else if (isname(lexer->ch)) {
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 0 };

        while (isname(lexer->ch)) {
            slice.len += 1;
            stack_lexer_read(lexer);
        }

        if (ds_string_slice_equals(&slice, &SLICE_FUNC)) {
            return (stack_token){.kind = STACK_TOKEN_FUNC, .value = {0}, .pos = position };
        } else if (ds_string_slice_equals(&slice, &SLICE_IN)) {
            return (stack_token){.kind = STACK_TOKEN_IN, .value = {0}, .pos = position };
        } else if (ds_string_slice_equals(&slice, &SLICE_END)) {
            return (stack_token){.kind = STACK_TOKEN_END, .value = {0}, .pos = position };
        } else if (ds_string_slice_equals(&slice, &SLICE_DATA)) {
            return (stack_token){.kind = STACK_TOKEN_DATA, .value = {0}, .pos = position };
        } else {
            return (stack_token){.kind = STACK_TOKEN_NAME, .value = slice, .pos = position };
        }
    } else {
        char *value = NULL;
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 1 };

        stack_lexer_read(lexer);

        return (stack_token){.kind = STACK_TOKEN_ILLEGAL, .value = slice, .pos = position };
    }
}

void stack_lexer_dump(stack_lexer *lexer) {
    stack_token token = {0};

    do {
        token = stack_lexer_next(lexer);
        char *value = stack_token_to_string(token);
        DS_LOG_INFO("%s", value);
        DS_FREE(NULL, value);
    } while (token.kind != STACK_TOKEN_EOF);
}

int stack_lexer_pos_to_lc(stack_lexer *lexer, unsigned int pos, unsigned int *line, unsigned int *col) {
    int result = 0;
    if (pos >= lexer->buffer_len) {
        return_defer(1);
    }

    *line = 1;
    *col = 1;

    for (unsigned int i = 0; i < pos; i++) {
        if (lexer->buffer[i] == '\n') {
            *line += 1;
            *col = 1;
        } else {
            *col += 1;
        }
    }

defer:
    return result;
}

void stack_lexer_free(stack_lexer *lexer) {
    lexer->buffer = NULL;
    lexer->buffer_len = 0;
    lexer->pos = 0;
    lexer->read_pos = 0;
    lexer->ch = 0;
}

typedef struct {
    stack_lexer lexer;
    stack_token tok;
    stack_token next_tok;
} stack_parser;

static stack_token stack_parser_peek(stack_parser *parser) {
    return parser->next_tok;
}

static stack_token stack_parser_read(stack_parser *parser) {
    parser->tok = stack_parser_peek(parser);

    parser->next_tok = stack_lexer_next(&parser->lexer);

    return parser->tok;
}

int stack_parser_init(stack_parser *parser, stack_lexer lexer) {
    parser->lexer = lexer;
    parser->tok = (stack_token){0};
    parser->next_tok = (stack_token){0};

    stack_parser_read(parser);

    return 0;
}

static void stack_parser_show_errorf(stack_parser *parser, const char *format, ...) {
    unsigned int line = 0;
    unsigned int col = 0;
    stack_lexer_pos_to_lc(&parser->lexer, parser->tok.pos, &line, &col);

    fprintf(stderr, ":%d:%d, ", line, col);
    if (parser->tok.kind == STACK_TOKEN_ILLEGAL) {
        fprintf(stderr, "Lexical error: ILLEGAL TOKEN");

        if (parser->tok.value.str != NULL) {
            char *value = NULL;
            ds_string_slice_to_owned(&parser->tok.value, &value);
            fprintf(stderr, ": %s", value);
            DS_FREE(NULL, value);
        }

        fprintf(stderr, "\n");
    } else {
        fprintf(stderr, "Syntax error: ");

        va_list args;
        va_start(args, format);
        vfprintf(stderr, format, args);
        va_end(args);

        fprintf(stderr, "\n");
    }
}

#define stack_parser_show_expected(parser, expected, got)                      \
  stack_parser_show_errorf(parser, "expected %s got %s",                       \
                           stack_token_kind_to_string(expected),               \
                           stack_token_kind_to_string(got))

#define stack_parser_show_expected_2(parser, expected1, expected2, got)        \
  stack_parser_show_errorf(parser, "expected %s or %s, got %s",                \
                           stack_token_kind_to_string(expected1),              \
                           stack_token_kind_to_string(expected2),              \
                           stack_token_kind_to_string(got))

#define stack_parser_show_expected_3(parser, expected1, expected2, expected3,  \
                                     got)                                      \
  stack_parser_show_errorf(parser, "expected %s, %s or %s, got %s",            \
                           stack_token_kind_to_string(expected1),              \
                           stack_token_kind_to_string(expected2),              \
                           stack_token_kind_to_string(expected3),              \
                           stack_token_kind_to_string(got))

void stack_parser_free(stack_parser *parser) {
    stack_lexer_free(&parser->lexer);
    parser->tok = (stack_token){0};
    parser->next_tok = (stack_token){0};
}

typedef struct {
    ds_string_slice value;
    unsigned int pos;
} stack_ast_node;

void stack_ast_node_free(stack_ast_node *node) {
    ds_string_slice_free(&node->value);
}

typedef struct {
    stack_ast_node type;
    stack_ast_node name;
} stack_ast_data_field;

static int stack_parser_parse_data_field(stack_parser *parser, stack_ast_data_field *field) {
    stack_token token = {0};
    int result = 0;

    field->type = (stack_ast_node){0};
    field->name = (stack_ast_node){0};

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        return_defer(1);
    }
    field->type = (stack_ast_node){.value = token.value, .pos = token.pos };

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        return_defer(1);
    }
    field->name = (stack_ast_node){.value = token.value, .pos = token.pos };

defer:
    return result;
}

void stack_ast_data_field_free(stack_ast_data_field *field) {
    stack_ast_node_free(&field->name);
    stack_ast_node_free(&field->type);
}

typedef struct {
    stack_ast_node name;
    ds_dynamic_array fields; // stack_ast_data_field
} stack_ast_data;

static int stack_parser_parse_data(stack_parser *parser, stack_ast_data *data) {
    stack_token token = {0};
    int result = 0;

    data->name = (stack_ast_node){0};
    ds_dynamic_array_init(&data->fields, sizeof(stack_ast_data_field));

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_DATA) {
        stack_parser_show_expected(parser, STACK_TOKEN_DATA, token.kind);
        return_defer(1);
    }

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        return_defer(1);
    }
    data->name = (stack_ast_node){.value = token.value, .pos = token.pos};

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_LPAREN) {
        stack_parser_show_expected(parser, STACK_TOKEN_LPAREN, token.kind);
        return_defer(1);
    }

    token = stack_parser_peek(parser);
    if (token.kind == STACK_TOKEN_RPAREN) {
        stack_parser_read(parser);
        return_defer(0);
    }

    do {
        stack_ast_data_field field = {0};
        stack_parser_parse_data_field(parser, &field);
        ds_dynamic_array_append(&data->fields, &field);

        token = stack_parser_read(parser);
        if (token.kind == STACK_TOKEN_RPAREN) {
            break;
        } else if (token.kind == STACK_TOKEN_COMMA) {
            continue;
        } else {
            stack_parser_show_expected_2(parser, STACK_TOKEN_COMMA, STACK_TOKEN_RPAREN, token.kind);
            return_defer(1);
        }
    } while (true);

defer:
    return result;
}

void stack_ast_data_free(stack_ast_data *data) {
    stack_ast_node_free(&data->name);
    for (unsigned int i = 0; i < data->fields.count; i++) {
        stack_ast_data_field *field = NULL;
        ds_dynamic_array_get_ref(&data->fields, i, (void **)&field);
        stack_ast_data_field_free(field);
    }
    ds_dynamic_array_free(&data->fields);
}

typedef enum {
    STACK_AST_EXPR_NUMBER,
    STACK_AST_EXPR_NAME,
} stack_ast_expr_kind;

typedef struct {
    stack_ast_expr_kind kind;
    union {
        stack_ast_node number;
        stack_ast_node name;
    };
} stack_ast_expr;

void stack_ast_expr_free(stack_ast_expr *expr) {
    switch (expr->kind) {
    case STACK_AST_EXPR_NUMBER:
        stack_ast_node_free(&expr->number);
        break;
    case STACK_AST_EXPR_NAME:
        stack_ast_node_free(&expr->name);
        break;
    }
}

typedef struct {
    stack_ast_node name;
    ds_dynamic_array args; // stack_ast_node
    ds_dynamic_array rets; // stack_ast_node
    ds_dynamic_array body; // stack_ast_expr
} stack_ast_func;

static int stack_parser_parse_func_nodes(stack_parser *parser, ds_dynamic_array *nodes /* stack_ast_node */) {
    stack_token token = {0};
    int result = 0;

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_LPAREN) {
        stack_parser_show_expected(parser, STACK_TOKEN_LPAREN, token.kind);
        return_defer(1);
    }

    token = stack_parser_peek(parser);
    if (token.kind == STACK_TOKEN_RPAREN) {
        stack_parser_read(parser);
        return_defer(0);
    }

    do {
        token = stack_parser_read(parser);
        if (token.kind != STACK_TOKEN_NAME) {
            stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
            return_defer(1);
        }
        ds_dynamic_array_append(nodes, &(stack_ast_node){.value = token.value, .pos = token.pos});

        token = stack_parser_read(parser);
        if (token.kind == STACK_TOKEN_RPAREN) {
            break;
        } else if (token.kind == STACK_TOKEN_COMMA) {
            continue;
        } else {
            stack_parser_show_expected_2(parser, STACK_TOKEN_COMMA, STACK_TOKEN_RPAREN, token.kind);
            return_defer(1);
        }
    } while (true);

defer:
    return result;
}

static int stack_parser_parse_func_body(stack_parser *parser, ds_dynamic_array *exprs /* stack_ast_expr */) {
    int result = 0;

    while (true) {
        stack_token token = stack_parser_peek(parser);
        switch (token.kind) {
        case STACK_TOKEN_NAME:
            stack_parser_read(parser);
            ds_dynamic_array_append(
                exprs,
                &(stack_ast_expr){.kind = STACK_AST_EXPR_NAME,
                                  .name = (stack_ast_node){.value = token.value,
                                                           .pos = token.pos}});
            break;
        case STACK_TOKEN_NUMBER:
            stack_parser_read(parser);
            ds_dynamic_array_append(
                exprs,
                &(stack_ast_expr){.kind = STACK_AST_EXPR_NUMBER,
                                  .number = (stack_ast_node){
                                      .value = token.value, .pos = token.pos}});
            break;
        default:
            return_defer(0);
        }
    }

defer:
    return result;
}

static int stack_parser_parse_func(stack_parser *parser, stack_ast_func *func) {
    stack_token token = {0};
    int result = 0;

    func->name = (stack_ast_node){0};
    ds_dynamic_array_init(&func->args, sizeof(stack_ast_node));
    ds_dynamic_array_init(&func->rets, sizeof(stack_ast_node));
    ds_dynamic_array_init(&func->body, sizeof(stack_ast_expr));

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_FUNC) {
        stack_parser_show_expected(parser, STACK_TOKEN_FUNC, token.kind);
        return_defer(1);
    }

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        return_defer(1);
    }
    func->name = (stack_ast_node){.value = token.value, .pos = token.pos};

    stack_parser_parse_func_nodes(parser, &func->args);
    stack_parser_parse_func_nodes(parser, &func->rets);

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_IN) {
        stack_parser_show_expected(parser, STACK_TOKEN_IN, token.kind);
        return_defer(1);
    }

    stack_parser_parse_func_body(parser, &func->body);

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_END) {
        stack_parser_show_expected(parser, STACK_TOKEN_END, token.kind);
        return_defer(1);
    }

defer:
    return result;
}

void stack_ast_func_free(stack_ast_func *func) {
    stack_ast_node_free(&func->name);
    for (unsigned int i = 0; i < func->args.count; i++) {
        stack_ast_node *arg = NULL;
        ds_dynamic_array_get_ref(&func->args, i, (void **)&arg);
        stack_ast_node_free(arg);
    }
    ds_dynamic_array_free(&func->args);

    for (unsigned int i = 0; i < func->rets.count; i++) {
        stack_ast_node *ret = NULL;
        ds_dynamic_array_get_ref(&func->rets, i, (void **)&ret);
        stack_ast_node_free(ret);
    }
    ds_dynamic_array_free(&func->rets);

    for (unsigned int i = 0; i < func->body.count; i++) {
        stack_ast_expr *expr = NULL;
        ds_dynamic_array_get_ref(&func->body, i, (void **)&expr);
        stack_ast_expr_free(expr);
    }
    ds_dynamic_array_free(&func->body);
}

typedef struct {
    ds_dynamic_array datas; // stack_ast_data
    ds_dynamic_array funcs; // stack_ast_func
} stack_ast_prog;

static int stack_parser_parse_prog(stack_parser *parser, stack_ast_prog *prog) {
    int result = 0;

    ds_dynamic_array_init(&prog->datas, sizeof(stack_ast_data));
    ds_dynamic_array_init(&prog->funcs, sizeof(stack_ast_func));

    while (true) {
        stack_token token = stack_parser_peek(parser);
        if (token.kind == STACK_TOKEN_DATA) {
            stack_ast_data data = {0};
            stack_parser_parse_data(parser, &data);
            ds_dynamic_array_append(&prog->datas, &data);
        } else if (token.kind == STACK_TOKEN_FUNC) {
            stack_ast_func func = {0};
            stack_parser_parse_func(parser, &func);
            ds_dynamic_array_append(&prog->funcs, &func);
        } else if (token.kind == STACK_TOKEN_EOF) {
            break;
        } else {
            stack_parser_show_expected_3(parser, STACK_TOKEN_DATA, STACK_TOKEN_FUNC, STACK_TOKEN_EOF, token.kind);
            return_defer(1);
        }
    }

defer:
    return result;
}

void stack_ast_dump(stack_ast_prog *prog, FILE* stdout) {
    const int indent = 2;

    for (unsigned int i = 0; i < prog->datas.count; i++) {
        char *name = NULL;

        stack_ast_data data = {0};
        ds_dynamic_array_get(&prog->datas, i, &data);

        ds_string_slice_to_owned(&data.name.value, &name);
        fprintf(stdout, "data %s\n", name);
        DS_FREE(NULL, name);

        for (unsigned int i = 0; i < data.fields.count; i++) {
            char *name = NULL;
            char *type = NULL;

            stack_ast_data_field field = {0};
            ds_dynamic_array_get(&data.fields, i, &field);

            ds_string_slice_to_owned(&field.type.value, &type);
            ds_string_slice_to_owned(&field.name.value, &name);
            fprintf(stdout, "%*s%s: %s\n", indent, "", name, type);
            DS_FREE(NULL, name);
            DS_FREE(NULL, type);
        }
        fprintf(stdout, "\n");
    }

    for (unsigned int i = 0; i < prog->funcs.count; i++) {
        char *name = NULL;

        stack_ast_func func = {0};
        ds_dynamic_array_get(&prog->funcs, i, &func);

        ds_string_slice_to_owned(&func.name.value, &name);
        fprintf(stdout, "func %s\n", name);
        DS_FREE(NULL, name);

        for (unsigned int j = 0; j < func.args.count; j++) {
            char *name = NULL;

            stack_ast_node arg = {0};
            ds_dynamic_array_get(&func.args, j, &arg);

            ds_string_slice_to_owned(&arg.value, &name);
            fprintf(stdout, "%*sarg%d: %s\n", indent, "", j, name);
            DS_FREE(NULL, name);
        }
        for (unsigned int j = 0; j < func.rets.count; j++) {
            char *name = NULL;

            stack_ast_node ret = {0};
            ds_dynamic_array_get(&func.rets, j, &ret);

            ds_string_slice_to_owned(&ret.value, &name);
            fprintf(stdout, "%*sret%d: %s\n", indent, "", j, name);
            DS_FREE(NULL, name);
        }

        fprintf(stdout, "%*sbody:\n", indent, "");
        for (unsigned int j = 0; j < func.body.count; j++) {
            char *name = NULL;

            stack_ast_expr expr = {0};
            ds_dynamic_array_get(&func.body, j, &expr);

            switch (expr.kind) {
            case STACK_AST_EXPR_NUMBER:
                ds_string_slice_to_owned(&expr.number.value, &name);
              break;
            case STACK_AST_EXPR_NAME:
                ds_string_slice_to_owned(&expr.name.value, &name);
              break;
            }
            fprintf(stdout, "%*s%s\n", indent * 2, "", name);
            DS_FREE(NULL, name);
        }
        fprintf(stdout, "\n");
    }
}

void stack_ast_free(stack_ast_prog *prog) {
    for (unsigned int i = 0; i < prog->datas.count; i++) {
        stack_ast_data *data = NULL;
        ds_dynamic_array_get_ref(&prog->datas, i, (void **)&data);
        stack_ast_data_free(data);
    }
    ds_dynamic_array_free(&prog->datas);

    for (unsigned int i = 0; i < prog->funcs.count; i++) {
        stack_ast_func *func = NULL;
        ds_dynamic_array_get_ref(&prog->funcs, i, (void **)&func);
        stack_ast_func_free(func);
    }
    ds_dynamic_array_free(&prog->funcs);
}

typedef struct {
    FILE *stdout;
} stack_assembler;

void stack_assembler_init(stack_assembler *assembler, FILE *stdout) {
    assembler->stdout = stdout;
}

static void stack_assembler_emit_format(stack_assembler *assembler, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(assembler->stdout, format, args);
    va_end(args);

    fprintf(assembler->stdout, "\n");
}

#define EMIT(format, ...) stack_assembler_emit_format(assembler, format, ##__VA_ARGS__)

static void stack_assembler_emit_entry(stack_assembler *assembler) {
    EMIT("section '.data' writeable");
    EMIT("");
    EMIT("; Define some constants");
    EMIT("stack_sz = 8");
    EMIT("");
    EMIT("loc_0 = 8");
    EMIT("loc_1 = 16");
    EMIT("loc_2 = 24");
    EMIT("loc_3 = 32");
    EMIT("loc_4 = 40");
    EMIT("loc_5 = 48");
    EMIT("loc_6 = 56");
    EMIT("loc_7 = 64");
    EMIT("");
    EMIT("arg_0 = 16");
    EMIT("arg_1 = 24");
    EMIT("arg_2 = 32");
    EMIT("arg_3 = 40");
    EMIT("arg_4 = 48");
    EMIT("arg_5 = 56");
    EMIT("arg_6 = 64");
    EMIT("arg_7 = 72");
    EMIT("");
    EMIT("; Define entry point");
    EMIT("section '.text' executable");
    EMIT("public _start");
    EMIT("_start:");
    EMIT("    ; Initialize the memory");
    EMIT("    call allocator_init");
    EMIT("");
    EMIT("    ; Call the main method");
    EMIT("    call   func.main");
    EMIT("");
    EMIT("    ; Exit the program");
    EMIT("    call    stack_pop");
    EMIT("    mov     rdi, qword [rax]");
    EMIT("    mov     rax, 60");
    EMIT("    syscall");
    EMIT("");
}

static void stack_assembler_emit_allocator(stack_assembler *assembler) {
    EMIT("section '.data' writeable");
    EMIT("");
    EMIT("; memory layout");
    EMIT("stack_pos dq 0");
    EMIT("stack_end dq 0");
    EMIT("heap_pos dq 0");
    EMIT("heap_end dq 0");
    EMIT("");
    EMIT("section '.text' executable");
    EMIT("");
    EMIT(";");
    EMIT(";");
    EMIT("; allocator_init");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("allocator_init:");
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("");
    EMIT("    ; allocate the stack 64K");
    EMIT("    mov     rax, 12                    ; brk");
    EMIT("    mov     rdi, 0                     ; increment = 0");
    EMIT("    syscall");
    EMIT("    mov     [stack_pos], rax           ; save the current position of the stack");
    EMIT("    mov     [stack_end], rax           ; save the end of the stack");
    EMIT("");
    EMIT("    mov     rax, 12                    ; brk");
    EMIT("    mov     rdi, 0x10000               ; 64K bytes (larger obj. will fail)");
    EMIT("    add     rdi, [stack_end]           ; new end of the stack");
    EMIT("    syscall");
    EMIT("");
    EMIT("    ; initialize the heap");
    EMIT("    mov     rax, 12                    ; brk");
    EMIT("    mov     rdi, 0                     ; increment = 0");
    EMIT("    syscall");
    EMIT("    mov     [heap_pos], rax            ; save the current position of the heap");
    EMIT("    mov     [heap_end], rax            ; save the end of the heap");
    EMIT("");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    EMIT(";");
    EMIT(";");
    EMIT("; stack push");
    EMIT(";");
    EMIT(";   INPUT: rdi contains the int64 (pointer) that we add to the stack");
    EMIT(";   OUTPUT: nothing");
    EMIT(";");
    EMIT("stack_push:");
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("");
    EMIT("    mov     rsi, qword [stack_pos]");
    EMIT("    mov     qword [rsi], rdi");
    EMIT("    add     qword [stack_pos], stack_sz");
    EMIT("");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    EMIT(";");
    EMIT(";");
    EMIT("; stack peek");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: rax contains the int64 (pointer) that we pop from the stack");
    EMIT(";");
    EMIT("stack_peek:");
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("");
    EMIT("    mov     rax, qword [stack_pos]");
    EMIT("    mov     rax, qword [rax - stack_sz]");
    EMIT("");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    EMIT(";");
    EMIT(";");
    EMIT("; stack pop");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: rax contains the int64 (pointer) that we pop from the stack");
    EMIT(";");
    EMIT("stack_pop:");
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("");
    EMIT("    mov     rax, qword [stack_pos]");
    EMIT("    mov     rax, qword [rax - stack_sz]");
    EMIT("    sub     qword [stack_pos], stack_sz");
    EMIT("");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    EMIT(";");
    EMIT(";");
    EMIT("; allocate");
    EMIT(";");
    EMIT(";   INPUT: rdi contains the size in bytes");
    EMIT(";   OUTPUT: rax points to the newly allocated memory");
    EMIT(";");
    EMIT("allocate:");
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    ; t0 <- heap_pos");
    EMIT("    mov     rax, qword [heap_pos]");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; t1 <- t0 + rdi");
    EMIT("    mov     rax, qword [rbp - loc_0]");
    EMIT("    add     rax, rdi");
    EMIT("    mov     qword [rbp - loc_1], rax");
    EMIT("");
    EMIT("    ; cmp t1 <= heap_end");
    EMIT("    mov     rax, qword [rbp - loc_1]");
    EMIT("    cmp     rax, qword [heap_end]");
    EMIT("    jle     .alloc_ok");
    EMIT("");
    EMIT("    mov     rax, 12                    ; brk");
    EMIT("    mov     rdi, 0x10000               ; 64K bytes (larger obj. will fail)");
    EMIT("    add     rdi, [heap_end]            ; new end of the heap");
    EMIT("    syscall");
    EMIT("");
    EMIT("    mov     [heap_end], rax            ; save the new end of the heap");
    EMIT("");
    EMIT(".alloc_ok:");
    EMIT("");
    EMIT("    ; heap_pos <- t1");
    EMIT("    mov     rax, qword [rbp - loc_1]");
    EMIT("    mov     qword [heap_pos], rax");
    EMIT("");
    EMIT("    ; return t0");
    EMIT("    mov     rax, qword [rbp - loc_0]");
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    EMIT(";");
    EMIT(";");
    EMIT("; memcpy");
    EMIT(";   INPUT:");
    EMIT(";       rdi points to destination");
    EMIT(";       rsi points to source");
    EMIT(";       rdx contains the number of bytes to copy");
    EMIT(";   STACK: empty");
    EMIT(";   OUTPUT: nothing");
    EMIT(";");
    EMIT("memcpy:");
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("");
    EMIT(".next_byte:");
    EMIT("    cmp     rdx, 0                     ; check if done");
    EMIT("    jle     .done");
    EMIT("");
    EMIT("    mov     al, byte [rsi]             ; get byte from self");
    EMIT("    mov     byte [rdi], al             ; copy byte to new object");
    EMIT("");
    EMIT("    inc     rdi                        ; increment destination");
    EMIT("    inc     rsi                        ; increment source");
    EMIT("    dec     rdx                        ; decrement count");
    EMIT("");
    EMIT("    jmp .next_byte");
    EMIT(".done:");
    EMIT("");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
}

#define STACK_KEYWORD_DUP "dup"
#define STACK_KEYWORD_SWP "swp"
#define STACK_KEYWORD_PLUS "+"
#define STACK_KEYWORD_STAR "*"

#define EXPR_NAME_DUP "func.dup"
#define EXPR_NAME_SWP "func.swp"
#define EXPR_NAME_INT "func.int"
#define EXPR_NAME_PLUS "func.int.plus"
#define EXPR_NAME_STAR "func.int.mul"

static const char *expr_name_map(char *name) {
    if (strcmp(STACK_KEYWORD_DUP, name) == 0) {
        return EXPR_NAME_DUP;
    } else if (strcmp(STACK_KEYWORD_SWP, name) == 0) {
        return EXPR_NAME_SWP;
    } else if (strcmp(STACK_KEYWORD_PLUS, name) == 0) {
        return EXPR_NAME_PLUS;
    } else if (strcmp(STACK_KEYWORD_STAR, name) == 0) {
        return EXPR_NAME_STAR;
    } else {
        return NULL;
    }
}

void stack_assembler_emit_stdlib(stack_assembler *assembler) {
    EMIT("section '.text' executable");
    EMIT("");
    // DUP
    EMIT(";");
    EMIT(";");
    EMIT("; dup");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("%s:", EXPR_NAME_DUP);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_peek");
    EMIT("    mov     rdi, rax");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // SWAP
    EMIT(";");
    EMIT(";");
    EMIT("; swp");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("%s:", EXPR_NAME_SWP);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    ; t0 <- A");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; t1 <- B");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_1], rax");
    EMIT("");
    EMIT("    ; push A");
    EMIT("    mov     rdi, [rbp - loc_0]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    ; push B");
    EMIT("    mov     rdi, [rbp - loc_1]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // INT
    EMIT(";");
    EMIT(";");
    EMIT("; int constructor");
    EMIT(";");
    EMIT(";   INPUT: rdi is the int value");
    EMIT(";   OUTPUT: nothing");
    EMIT("%s:", EXPR_NAME_INT);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    ; t1 <- rdi");
    EMIT("    mov     qword [rbp - loc_1], rdi");
    EMIT("");
    EMIT("    ; t0 <- allocate(8)");
    EMIT("    mov     rdi, 8");
    EMIT("    call    allocate");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; *t0 <- t1");
    EMIT("    mov     rax, [rbp - loc_1]");
    EMIT("    mov     rdi, [rbp - loc_0]");
    EMIT("    mov     qword [rdi], rax");
    EMIT("");
    EMIT("    ; push t0");
    EMIT("    mov     rdi, [rbp - loc_0]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // PLUS
    EMIT(";");
    EMIT(";");
    EMIT("; plus");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("%s:", EXPR_NAME_PLUS);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    push    rax");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    pop     rdi");
    EMIT("");
    EMIT("    add     rdi, rax");
    EMIT("    call    %s", EXPR_NAME_INT);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
}

void stack_assembler_emit_data(stack_assembler *assembler, stack_ast_data *data) {
}

void stack_assembler_emit_expr_number(stack_assembler *assembler, stack_ast_node *node) {
    char *number = NULL;
    ds_string_slice_to_owned(&node->value, &number);
    EMIT("    mov     rdi, %s", number);
    EMIT("    call    %s", EXPR_NAME_INT);

    DS_FREE(NULL, number);
}

void stack_assembler_emit_expr_name(stack_assembler *assembler, stack_ast_node *node) {
    char *name = NULL;
    ds_string_slice_to_owned(&node->value, &name);
    const char *func = expr_name_map(name);

    if (func == NULL) {
        EMIT("    call    func.%s", name);
    } else {
        EMIT("    call    %s", func);
    }

    DS_FREE(NULL, name);
}

void stack_assembler_emit_expr(stack_assembler *assembler, stack_ast_expr *expr) {
    switch (expr->kind) {
    case STACK_AST_EXPR_NUMBER:
        stack_assembler_emit_expr_number(assembler, &expr->number);
        break;
    case STACK_AST_EXPR_NAME:
        stack_assembler_emit_expr_name(assembler, &expr->name);
        break;
    }
}

void stack_assembler_emit_func(stack_assembler *assembler, stack_ast_func *func) {
    char *name = NULL;
    ds_string_slice_to_owned(&func->name.value, &name);

    EMIT("func.%s:", name);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");

    for (unsigned int i = 0; i < func->body.count; i++) {
        stack_ast_expr expr = {0};
        ds_dynamic_array_get(&func->body, i, &expr);
        stack_assembler_emit_expr(assembler, &expr);
    }

    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");

    DS_FREE(NULL, name);
}

void stack_assembler_emit(stack_assembler *assembler, stack_ast_prog *prog) {
    EMIT("format ELF64");
    EMIT("");
    stack_assembler_emit_allocator(assembler);
    stack_assembler_emit_entry(assembler);
    stack_assembler_emit_stdlib(assembler);

    EMIT("section '.text' executable");
    EMIT("");

    for (unsigned int i = 0; i < prog->datas.count; i++) {
        stack_ast_data data = {0};
        ds_dynamic_array_get(&prog->datas, i, &data);
        stack_assembler_emit_data(assembler, &data);
    }

    for (unsigned int i = 0; i < prog->funcs.count; i++) {
        stack_ast_func func = {0};
        ds_dynamic_array_get(&prog->funcs, i, &func);
        stack_assembler_emit_func(assembler, &func);
    }
}

void stack_assembler_free(stack_assembler *assembler) {

}

typedef struct {
    char *filename;
} t_args;

static int argparse(int argc, char **argv, t_args *args) {
    int result = 0;
    ds_argparse_parser argparser = {0};

    ds_argparse_parser_init(&argparser, "stack", "stack lang compiler" , "0.1");

    if (ds_argparse_add_argument(&argparser, (ds_argparse_options){
        .short_name = 'i',
        .long_name = "input",
        .description = "the input file",
        .type = ARGUMENT_TYPE_POSITIONAL,
        .required = 0,
    }) != 0) {
        DS_LOG_ERROR("Failed to add argument `input`");
        return_defer(1);
    }

    if (ds_argparse_parse(&argparser, argc, argv) != 0) {
        DS_LOG_ERROR("Failed to parse arguments");
        return_defer(1);
    }

    args->filename = ds_argparse_get_value(&argparser, "input");

defer:
    ds_argparse_parser_free(&argparser);
    return result;
}

int main(int argc, char **argv) {
    char *buffer = NULL;
    unsigned int buffer_len = 0;
    t_args args = {0};
    stack_lexer lexer = {0};
    stack_parser parser = {0};
    stack_ast_prog prog = {0};
    stack_assembler assembler = {0};
    int result = 0;

    if (argparse(argc, argv, &args) != 0) {
        DS_LOG_ERROR("Failed to parse arguments");
        return_defer(1);
    }

    buffer_len = ds_io_read(args.filename, &buffer, "r");
    if (buffer_len < 0) {
        DS_LOG_ERROR("Failed to read from file: %s", (args.filename == NULL) ? "stdin" : args.filename);
        return_defer(1);
    }

    stack_lexer_init(&lexer, buffer, buffer_len);
    stack_parser_init(&parser, lexer);
    stack_assembler_init(&assembler, stdout);

    if (stack_parser_parse_prog(&parser, &prog) != 0) {
        return_defer(1);
    }

    stack_assembler_emit(&assembler, &prog);
    // stack_ast_dump(&prog, stdout);
    // stack_lexer_dump(&lexer);

defer:
    if (buffer != NULL) DS_FREE(NULL, buffer);
    stack_parser_free(&parser);
    stack_ast_free(&prog);
    return result;
}
