#define DS_IO_IMPLEMENTATION
#define DS_AP_IMPLEMENTATION
#define DS_SS_IMPLEMENTATION
#define DS_SB_IMPLEMENTATION
#define DS_DA_IMPLEMENTATION
#include "ds.h"

#define OOM_ERROR "Buy more RAM!"

#define SYMBOL_COMMA ','
#define SYMBOL_MINUS '-'
#define SYMBOL_GT '>'
#define SYMBOL_LT '>'
#define SYMBOL_EQ '='
#define SYMBOL_LPAREN '('
#define SYMBOL_RPAREN ')'
#define SYMBOL_NEWLINE '\n'

#define isname(c) ((isalnum((c)) || ispunct((c))) && (c) != SYMBOL_LPAREN && (c) != SYMBOL_RPAREN && (c) != SYMBOL_COMMA)

#define SLICE_FUNC DS_STRING_SLICE("func")
#define SLICE_IN DS_STRING_SLICE("in")
#define SLICE_END DS_STRING_SLICE("end")
#define SLICE_DATA DS_STRING_SLICE("data")
#define SLICE_TRUE DS_STRING_SLICE("true")
#define SLICE_FALSE DS_STRING_SLICE("false")

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
    STACK_TOKEN_BOOLEAN,
    STACK_TOKEN_EOF,
    STACK_TOKEN_ILLEGAL,
} stack_token_kind;

static const char* stack_token_kind_map(stack_token_kind kind) {
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
    case STACK_TOKEN_BOOLEAN: return "BOOLEAN";
    case STACK_TOKEN_EOF: return "<EOF>";
    case STACK_TOKEN_ILLEGAL: return "ILLEGAL";
    }
}

typedef struct {
    stack_token_kind kind;
    ds_string_slice value;
    unsigned int pos;
} stack_token;

#define STACK_TOKEN(k, v, p) (stack_token){.kind = (k), .value = (v), .pos = (p)}

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

static void stack_lexer_skip_until_newline(stack_lexer *lexer) {
    while (lexer->ch != SYMBOL_NEWLINE && lexer->ch != EOF) {
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
        return STACK_TOKEN(STACK_TOKEN_EOF, (ds_string_slice){0}, position);
    } else if (lexer->ch == SYMBOL_LPAREN) {
        stack_lexer_read(lexer);
        return STACK_TOKEN(STACK_TOKEN_LPAREN, (ds_string_slice){0}, position);
    } else if (lexer->ch == SYMBOL_RPAREN) {
        stack_lexer_read(lexer);
        return STACK_TOKEN(STACK_TOKEN_RPAREN, (ds_string_slice){0}, position);
    } else if (lexer->ch == SYMBOL_COMMA) {
        stack_lexer_read(lexer);
        return STACK_TOKEN(STACK_TOKEN_COMMA, (ds_string_slice){0}, position);
    } else if ((lexer->ch == SYMBOL_MINUS && stack_lexer_peek(lexer) == SYMBOL_MINUS)) {
        stack_lexer_skip_until_newline(lexer);
        return stack_lexer_next(lexer);
    } else if (isdigit(lexer->ch) || (lexer->ch == SYMBOL_MINUS && isdigit(stack_lexer_peek(lexer)))) {
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 0 };

        if (lexer->ch == SYMBOL_MINUS) {
            slice.len += 1;
            stack_lexer_read(lexer);
        }

        while (isdigit(lexer->ch)) {
            slice.len += 1;
            stack_lexer_read(lexer);
        }

        return STACK_TOKEN(STACK_TOKEN_NUMBER, slice, position);
    } else if (isname(lexer->ch)) {
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 0 };

        while (isname(lexer->ch)) {
            slice.len += 1;
            stack_lexer_read(lexer);
        }

        if (ds_string_slice_equals(&slice, &SLICE_FUNC)) {
            return STACK_TOKEN(STACK_TOKEN_FUNC, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_IN)) {
            return STACK_TOKEN(STACK_TOKEN_IN, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_END)) {
            return STACK_TOKEN(STACK_TOKEN_END, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_DATA)) {
            return STACK_TOKEN(STACK_TOKEN_DATA, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_TRUE) || ds_string_slice_equals(&slice, &SLICE_FALSE)) {
            return STACK_TOKEN(STACK_TOKEN_BOOLEAN, slice, position);
        } else {
            return STACK_TOKEN(STACK_TOKEN_NAME, slice, position);
        }
    } else {
        char *value = NULL;
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 1 };

        stack_lexer_read(lexer);

        return STACK_TOKEN(STACK_TOKEN_ILLEGAL, slice, position);
    }
}

void stack_lexer_dump(stack_lexer *lexer) {
    stack_token token = {0};

    do {
        token = stack_lexer_next(lexer);

        fprintf(stdout, "%s", stack_token_kind_map(token.kind));
        if (token.value.str != NULL) {
            fprintf(stdout, "(%.*s)", token.value.len, token.value.str);
        }
        fprintf(stdout, "\n");
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

    char *filename;
} stack_parser;

static stack_token stack_parser_peek(stack_parser *parser) {
    return parser->next_tok;
}

static stack_token stack_parser_read(stack_parser *parser) {
    parser->tok = stack_parser_peek(parser);

    parser->next_tok = stack_lexer_next(&parser->lexer);

    return parser->tok;
}

int stack_parser_init(stack_parser *parser, stack_lexer lexer, char *filename) {
    parser->lexer = lexer;
    parser->tok = (stack_token){0};
    parser->next_tok = (stack_token){0};
    parser->filename = filename;

    stack_parser_read(parser);

    return 0;
}

static void stack_parser_show_errorf(stack_parser *parser, const char *format, ...) {
    unsigned int line = 0;
    unsigned int col = 0;
    stack_lexer_pos_to_lc(&parser->lexer, parser->tok.pos, &line, &col);

    if (parser->filename != NULL) {
        fprintf(stderr, "%s", parser->filename);
    }

    fprintf(stderr, ":%d:%d, ", line, col);
    if (parser->tok.kind == STACK_TOKEN_ILLEGAL) {
        fprintf(stderr, "Lexical error: ILLEGAL TOKEN");

        if (parser->tok.value.str != NULL) {
            fprintf(stderr, ": (%.*s)", parser->tok.value.len, parser->tok.value.str);
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
                           stack_token_kind_map(expected),                     \
                           stack_token_kind_map(got))

#define stack_parser_show_expected_2(parser, expected1, expected2, got)        \
  stack_parser_show_errorf(                                                    \
      parser, "expected %s or %s, got %s", stack_token_kind_map(expected1),    \
      stack_token_kind_map(expected2), stack_token_kind_map(got))

#define stack_parser_show_expected_3(parser, expected1, expected2, expected3,  \
                                     got)                                      \
  stack_parser_show_errorf(                                                    \
      parser, "expected %s, %s or %s, got %s",                                 \
      stack_token_kind_map(expected1), stack_token_kind_map(expected2),        \
      stack_token_kind_map(expected3), stack_token_kind_map(got))

void stack_parser_free(stack_parser *parser) {
    stack_lexer_free(&parser->lexer);
    parser->tok = (stack_token){0};
    parser->next_tok = (stack_token){0};

    parser->filename = NULL;
}

typedef struct {
    ds_string_slice value;
    stack_parser *parser;
    unsigned int pos;
} stack_ast_node;

#define STACK_AST_NODE(v, p, s) ((stack_ast_node){.value = (v), .parser = (p), .pos = (s)})

static void stack_ast_node_free(stack_ast_node *node) {
    ds_string_slice_free(&node->value);
    node->parser = NULL;
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
    field->type = STACK_AST_NODE(token.value, parser, token.pos);

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        return_defer(1);
    }
    field->name = STACK_AST_NODE(token.value, parser, token.pos);

defer:
    return result;
}

static void stack_ast_data_field_free(stack_ast_data_field *field) {
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
    data->name = STACK_AST_NODE(token.value, parser, token.pos);

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
        if (stack_parser_parse_data_field(parser, &field) != 0) {
            return_defer(1);
        }
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

static void stack_ast_data_free(stack_ast_data *data) {
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
    STACK_AST_EXPR_BOOLEAN,
    STACK_AST_EXPR_NAME,
} stack_ast_expr_kind;

typedef struct {
    stack_ast_expr_kind kind;
    union {
        stack_ast_node number;
        stack_ast_node boolean;
        stack_ast_node name;
    };
} stack_ast_expr;

#define STACK_AST_EXPR_NUMBER(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_NUMBER, .number = (node)}
#define STACK_AST_EXPR_BOOLEAN(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_BOOLEAN, .boolean = (node)}
#define STACK_AST_EXPR_NAME(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_NAME, .name = (node)}

static int stack_parser_parse_expr(stack_parser *parser, stack_ast_expr *expr) {
    int result = 0;

    stack_token token = stack_parser_peek(parser);
    switch (token.kind) {
    case STACK_TOKEN_NAME:
        stack_parser_read(parser);
        *expr = STACK_AST_EXPR_NAME(STACK_AST_NODE(token.value, parser, token.pos));
        return_defer(0);
    case STACK_TOKEN_NUMBER:
        stack_parser_read(parser);
        *expr = STACK_AST_EXPR_NUMBER(STACK_AST_NODE(token.value, parser, token.pos));
        return_defer(0);
    case STACK_TOKEN_BOOLEAN:
        stack_parser_read(parser);
        *expr = STACK_AST_EXPR_BOOLEAN(STACK_AST_NODE(token.value, parser, token.pos));
        return_defer(0);
    default:
        return_defer(1);
    }

defer:
    return result;
}

static void stack_ast_expr_free(stack_ast_expr *expr) {
    switch (expr->kind) {
    case STACK_AST_EXPR_NUMBER:
        stack_ast_node_free(&expr->number);
        break;
    case STACK_AST_EXPR_NAME:
        stack_ast_node_free(&expr->name);
        break;
    case STACK_AST_EXPR_BOOLEAN:
        stack_ast_node_free(&expr->boolean);
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
        ds_dynamic_array_append(nodes, &STACK_AST_NODE(token.value, parser, token.pos));

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
    func->name = STACK_AST_NODE(token.value, parser, token.pos);

    stack_parser_parse_func_nodes(parser, &func->args);
    stack_parser_parse_func_nodes(parser, &func->rets);

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_IN) {
        stack_parser_show_expected(parser, STACK_TOKEN_IN, token.kind);
        return_defer(1);
    }

    do {
        token = stack_parser_peek(parser);
        if (token.kind == STACK_TOKEN_END) {
            stack_parser_read(parser);
            return_defer(0);
        }

        stack_ast_expr expr = {0};
        if (stack_parser_parse_expr(parser, &expr) != 0) {
            stack_parser_show_expected(parser, STACK_TOKEN_END, token.kind);
            return_defer(1);
        }
        ds_dynamic_array_append(&func->body, &expr);
    } while (true);

defer:
    return result;
}

static void stack_ast_func_free(stack_ast_func *func) {
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

int stack_parser_parse(stack_parser *parser, stack_ast_prog *prog) {
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
        stack_ast_data data = {0};
        ds_dynamic_array_get(&prog->datas, i, &data);

        fprintf(stdout, "data %.*s\n", data.name.value.len, data.name.value.str);
        for (unsigned int i = 0; i < data.fields.count; i++) {
            stack_ast_data_field field = {0};
            ds_dynamic_array_get(&data.fields, i, &field);

            fprintf(stdout, "%*s%.*s: %.*s\n", indent, "", field.name.value.len, field.name.value.str, field.type.value.len, field.type.value.str);
        }
        fprintf(stdout, "\n");
    }

    for (unsigned int i = 0; i < prog->funcs.count; i++) {
        stack_ast_func func = {0};
        ds_dynamic_array_get(&prog->funcs, i, &func);

        fprintf(stdout, "func %.*s\n", func.name.value.len, func.name.value.str);
        for (unsigned int j = 0; j < func.args.count; j++) {
            stack_ast_node arg = {0};
            ds_dynamic_array_get(&func.args, j, &arg);

            fprintf(stdout, "%*sarg%d: %.*s\n", indent, "", j, arg.value.len, arg.value.str);
        }
        for (unsigned int j = 0; j < func.rets.count; j++) {
            stack_ast_node ret = {0};
            ds_dynamic_array_get(&func.rets, j, &ret);

            fprintf(stdout, "%*sret%d: %.*s\n", indent, "", j, ret.value.len, ret.value.str);
        }

        fprintf(stdout, "%*sbody:\n", indent, "");
        for (unsigned int j = 0; j < func.body.count; j++) {
            ds_string_slice slice = {0};

            stack_ast_expr expr = {0};
            ds_dynamic_array_get(&func.body, j, &expr);

            switch (expr.kind) {
            case STACK_AST_EXPR_NUMBER:
                slice = expr.number.value;
                break;
            case STACK_AST_EXPR_BOOLEAN:
                slice = expr.boolean.value;
                break;
            case STACK_AST_EXPR_NAME:
                slice = expr.name.value;
                break;
            }
            fprintf(stdout, "%*s%.*s\n", indent * 2, "", slice.len, slice.str);
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

// TODO: BIG TODO HERE. I need to think more about the ASM part. More optimized.
// Function names. Etc.

#define STACK_DATA_INT "int"
#define STACK_DATA_BOOL "bool"

#define STACK_FUNC_MAIN "main"
#define STACK_FUNC_DUP "dup"
#define STACK_FUNC_SWP "swp"
#define STACK_FUNC_ROT "rot"
#define STACK_FUNC_POP "pop"
#define STACK_FUNC_PLUS "+"
#define STACK_FUNC_STAR "*"
#define STACK_FUNC_GT ">"
#define STACK_FUNC_LT "<"
#define STACK_FUNC_EQ "="

typedef struct {
    // TODO: implement data structure for the context:
    // - list with functions
    // - list with data structures
    int a;
} stack_context;

// TODO: add functions to add new funcs and datas

void stack_context_init(stack_context *context) {
    // TODO: add the implicit functions: +, *, dup...
    // TODO: add the implicit data types with associated functions (cons and get)
}

void stack_context_free(stack_context *context) {
    // TODO: implement free
}

typedef struct {
    char *filename;
} stack_typechecker;

void stack_typechecker_init(stack_typechecker *typechecker, char *filename) {
    typechecker->filename = filename;
}

int stack_typechecker_check(stack_typechecker *typechecker, stack_ast_prog *prog, stack_context *context) {
    int result = 0;

    // TODO: implement
    // - simulate the stack with types.
    // - how about generic types? dup is func (a) (a a) => inference
    // - how about "traits" Eq, Cmp, etc.
    return_defer(0);

defer:
    return result;
}

void stack_typechecker_free(stack_typechecker *typechecker) {
    typechecker->filename = NULL;
}

#define STACK_WORD_SZ 8

typedef struct {
    ds_dynamic_array func_map;
    ds_dynamic_array _data_bufs;
    FILE *stdout;
} stack_assembler;

static unsigned long stack_assembler_func_map(stack_assembler *assembler, ds_string_slice* name) {
    for (unsigned int i = 0; i < assembler->func_map.count; i++) {
        ds_string_slice item = {0};
        ds_dynamic_array_get(&assembler->func_map, i, &item);
        if (ds_string_slice_equals(name, &item)) {
            return i;
        }
    }

    ds_dynamic_array_append(&assembler->func_map, name);
    return assembler->func_map.count - 1;
}

void stack_assembler_init(stack_assembler *assembler, FILE *stdout) {
    ds_dynamic_array_init(&assembler->func_map, sizeof(ds_string_slice));
    ds_dynamic_array_init(&assembler->_data_bufs, sizeof(char *));
    assembler->stdout = stdout;
}

// TODO: maybe refactor big chunks of EMIT's into things like `EMIT_POP`
#define EMIT(format, ...) fprintf(assembler->stdout, format "\n", ##__VA_ARGS__)

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
    EMIT("    call   func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_MAIN)), STACK_FUNC_MAIN);
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
}

static void stack_assembler_emit_keywords(stack_assembler *assembler) {
    EMIT("section '.text' executable");
    EMIT("");
    // DUP
    EMIT(";");
    EMIT(";");
    EMIT("; dup");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_DUP)), STACK_FUNC_DUP);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_SWP)), STACK_FUNC_SWP);
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
    // ROT
    EMIT(";");
    EMIT(";");
    EMIT("; rot");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_ROT)), STACK_FUNC_ROT);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 32                    ; allocate 4 local variables");
    EMIT("");
    EMIT("    ; (C B A) -> (B A C)");
    EMIT("");
    EMIT("    ; t0 <- A");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; t1 <- B");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_1], rax");
    EMIT("");
    EMIT("    ; t2 <- C");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_2], rax");
    EMIT("");
    EMIT("    ; push B");
    EMIT("    mov     rdi, [rbp - loc_1]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    ; push A");
    EMIT("    mov     rdi, [rbp - loc_0]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    ; push C");
    EMIT("    mov     rdi, [rbp - loc_2]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 32                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // POP
    EMIT(";");
    EMIT(";");
    EMIT("; pop");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_POP)), STACK_FUNC_POP);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    EMIT("");
    // INT
    EMIT(";");
    EMIT(";");
    EMIT("; int constructor");
    EMIT(";");
    EMIT(";   INPUT: rdi is the int value");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_INT)), STACK_DATA_INT);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    ; t1 <- rdi");
    EMIT("    mov     qword [rbp - loc_1], rdi");
    EMIT("");
    EMIT("    ; t0 <- allocate(%d)", STACK_WORD_SZ);
    EMIT("    mov     rdi, %d", STACK_WORD_SZ);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_PLUS)), STACK_FUNC_PLUS);
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
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_INT)), STACK_DATA_INT);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // MUL
    EMIT(";");
    EMIT(";");
    EMIT("; mul");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_STAR)), STACK_FUNC_STAR);
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
    EMIT("    mul     rdi");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_INT)), STACK_DATA_INT);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // GT
    EMIT(";");
    EMIT(";");
    EMIT("; greater than");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_GT)), STACK_FUNC_GT);
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
    EMIT("    cmp     rdi, rax");
    EMIT("    setg    al");
    EMIT("    and     al, 1");
    EMIT("    movzx   rax, al");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_BOOL)), STACK_DATA_BOOL);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // LT
    EMIT(";");
    EMIT(";");
    EMIT("; less than");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_LT)), STACK_FUNC_LT);
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
    EMIT("    cmp     rdi, rax");
    EMIT("    setl    al");
    EMIT("    and     al, 1");
    EMIT("    movzx   rax, al");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_BOOL)), STACK_DATA_BOOL);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // EQ
    EMIT(";");
    EMIT(";");
    EMIT("; less than");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_FUNC_EQ)), STACK_FUNC_EQ);
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
    EMIT("    cmp     rdi, rax");
    EMIT("    sete    al");
    EMIT("    and     al, 1");
    EMIT("    movzx   rax, al");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_BOOL)), STACK_DATA_BOOL);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // BOOL
    EMIT(";");
    EMIT(";");
    EMIT("; bool constructor");
    EMIT(";");
    EMIT(";   INPUT: rdi is the bool value");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_BOOL)), STACK_DATA_BOOL);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    ; t1 <- rdi");
    EMIT("    mov     qword [rbp - loc_1], rdi");
    EMIT("");
    EMIT("    ; t0 <- allocate(%d)", STACK_WORD_SZ);
    EMIT("    mov     rdi, %d", STACK_WORD_SZ);
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
}

static void stack_assembler_emit_data(stack_assembler *assembler, stack_ast_data *data) {
    EMIT("func.%lu: ; %.*s", stack_assembler_func_map(assembler, &data->name.value), data->name.value.len, data->name.value.str);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");

    unsigned int size = data->fields.count * STACK_WORD_SZ;
    EMIT("    ; t0 <- allocate(%d)", size);
    EMIT("    mov     rdi, %d", size);
    EMIT("    call    allocate");
    EMIT("    mov     qword [rbp - loc_0], rax");

    for (unsigned int i = 0; i < data->fields.count; i++) {
        stack_ast_data_field field = {0};
        ds_dynamic_array_get(&data->fields, i, &field);

        EMIT("    ; *t0.%.*s <- pop", field.name.value.len, field.name.value.str);
        EMIT("    call    stack_pop");
        EMIT("    mov     rdi, [rbp - loc_0]");
        EMIT("    mov     qword [rdi + %d], rax", i * STACK_WORD_SZ);
    }

    EMIT("    ; push t0");
    EMIT("    mov     rdi, [rbp - loc_0]");
    EMIT("    call    stack_push");

    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");

    for (unsigned int i = 0; i < data->fields.count; i++) {
        char *buffer = NULL;
        ds_string_builder sb = {0};
        ds_string_slice slice = {0};
        stack_ast_data_field field = {0};

        ds_dynamic_array_get(&data->fields, i, &field);

        // TODO: maybe can use the context to not allocate it again
        ds_string_builder_init(&sb);
        if (ds_string_builder_append(&sb, "%.*s.%.*s", data->name.value.len, data->name.value.str, field.name.value.len, field.name.value.str) != 0) {
            DS_PANIC(OOM_ERROR);
        }
        if (ds_string_builder_build(&sb, &buffer) != 0) {
            DS_PANIC(OOM_ERROR);
        }
        ds_dynamic_array_append(&assembler->_data_bufs, &buffer);

        slice = DS_STRING_SLICE(buffer);

        EMIT("func.%lu: ; %.*s", stack_assembler_func_map(assembler, &slice), slice.len, slice.str);
        EMIT("    push    rbp                        ; save return address");
        EMIT("    mov     rbp, rsp                   ; set up stack frame");

        EMIT("    call    stack_pop");
        EMIT("    mov     rax, [rax + %d]", i * STACK_WORD_SZ);
        EMIT("    mov     rdi, rax");
        EMIT("    call    stack_push");

        EMIT("    pop     rbp                        ; restore return address");
        EMIT("    ret");
        EMIT("");

        ds_string_slice_free(&slice);
        ds_string_builder_free(&sb);
    }
}

void stack_assembler_emit_expr_number(stack_assembler *assembler, stack_ast_node *node) {
    EMIT("    mov     rdi, %.*s", node->value.len, node->value.str);
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_INT)), STACK_DATA_INT);
}

void stack_assembler_emit_expr_boolean(stack_assembler *assembler, stack_ast_node *node) {
    EMIT("    mov     rdi, %d", ds_string_slice_equals(&node->value, &SLICE_TRUE) ? 1 : 0);
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &DS_STRING_SLICE(STACK_DATA_BOOL)), STACK_DATA_BOOL);
}

void stack_assembler_emit_expr_name(stack_assembler *assembler, stack_ast_node *node) {
    EMIT("    call    func.%lu ; %.*s", stack_assembler_func_map(assembler, &node->value), node->value.len, node->value.str);
}

void stack_assembler_emit_expr(stack_assembler *assembler, stack_ast_expr *expr) {
    switch (expr->kind) {
    case STACK_AST_EXPR_NUMBER:
        stack_assembler_emit_expr_number(assembler, &expr->number);
        break;
    case STACK_AST_EXPR_BOOLEAN:
        stack_assembler_emit_expr_boolean(assembler, &expr->boolean);
        break;
    case STACK_AST_EXPR_NAME:
        stack_assembler_emit_expr_name(assembler, &expr->name);
        break;
    }
}

static void stack_assembler_emit_func(stack_assembler *assembler, stack_ast_func *func) {
    EMIT("func.%lu: ; %.*s", stack_assembler_func_map(assembler, &func->name.value), func->name.value.len, func->name.value.str);
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
}

void stack_assembler_emit(stack_assembler *assembler, stack_ast_prog *prog, stack_context *context) {
    EMIT("format ELF64");
    EMIT("");
    stack_assembler_emit_allocator(assembler);
    stack_assembler_emit_entry(assembler);
    stack_assembler_emit_keywords(assembler);

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
    ds_dynamic_array_free(&assembler->func_map);
    for (unsigned int i = 0; i < assembler->_data_bufs.count; i++) {
        char *buffer = NULL;
        ds_dynamic_array_get(&assembler->_data_bufs, i, &buffer);
        DS_FREE(NULL, buffer);
    }
    ds_dynamic_array_free(&assembler->_data_bufs);
    assembler->stdout = NULL;
}

typedef struct {
    char *filename;
    bool lexer;
    bool parser;
    bool typecheck;
    bool assembler;
} t_args;

static int argparse(int argc, char **argv, t_args *args) {
    int result = 0;
    ds_argparse_parser argparser = {0};

    ds_argparse_parser_init(&argparser, "stack", "stack lang compiler" , "0.1");

    if (ds_argparse_add_argument(&argparser, (ds_argparse_options){
        .short_name = 'l',
        .long_name = "lexer",
        .description = "flag to stop on the lexer phase",
        .type = ARGUMENT_TYPE_FLAG,
        .required = 0,
    }) != 0) {
        DS_LOG_ERROR("Failed to add argument `lexer`");
        return_defer(1);
    }

    if (ds_argparse_add_argument(&argparser, (ds_argparse_options){
        .short_name = 'p',
        .long_name = "parser",
        .description = "flag to stop on the parser phase",
        .type = ARGUMENT_TYPE_FLAG,
        .required = 0,
    }) != 0) {
        DS_LOG_ERROR("Failed to add argument `parser`");
        return_defer(1);
    }

    if (ds_argparse_add_argument(&argparser, (ds_argparse_options){
        .short_name = 'a',
        .long_name = "assembler",
        .description = "flag to stop on the assembler phase",
        .type = ARGUMENT_TYPE_FLAG,
        .required = 0,
    }) != 0) {
        DS_LOG_ERROR("Failed to add argument `assembler`");
        return_defer(1);
    }

    if (ds_argparse_add_argument(&argparser, (ds_argparse_options){
        .short_name = 't',
        .long_name = "typecheck",
        .description = "flag to stop on the typecheck phase",
        .type = ARGUMENT_TYPE_FLAG,
        .required = 0,
    }) != 0) {
        DS_LOG_ERROR("Failed to add argument `typecheck`");
        return_defer(1);
    }

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
    args->lexer = ds_argparse_get_flag(&argparser, "lexer");
    args->parser = ds_argparse_get_flag(&argparser, "parser");
    args->typecheck = ds_argparse_get_flag(&argparser, "typecheck");
    args->assembler = ds_argparse_get_flag(&argparser, "assembler");

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
    stack_typechecker typechecker = {0};
    stack_assembler assembler = {0};
    stack_ast_prog prog = {0};
    stack_context context = {0};
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
    stack_parser_init(&parser, lexer, args.filename);
    stack_context_init(&context);
    stack_typechecker_init(&typechecker, args.filename);
    stack_assembler_init(&assembler, stdout);

    if (args.lexer) {
        stack_lexer_dump(&lexer);
        return_defer(0);
    }

    if (stack_parser_parse(&parser, &prog) != 0) {
        return_defer(1);
    }

    if (args.parser) {
        stack_ast_dump(&prog, stdout);
        return_defer(0);
    }

    // TODO: dependency graph based on `import` and merge all the ASTs

    if (stack_typechecker_check(&typechecker, &prog, &context) != 0) {
        return_defer(1);
    }

    if (args.typecheck) {
        return_defer(0);
    }

    if (args.assembler) {
        stack_assembler_emit(&assembler, &prog, &context);
        return_defer(0);
    }

defer:
    stack_assembler_free(&assembler);
    stack_ast_free(&prog);
    stack_parser_free(&parser);
    if (buffer != NULL) DS_FREE(NULL, buffer);
    return result;
}
