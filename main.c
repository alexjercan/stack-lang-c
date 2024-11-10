#define DS_IO_IMPLEMENTATION
#define DS_AP_IMPLEMENTATION
#define DS_SS_IMPLEMENTATION
#define DS_SB_IMPLEMENTATION
#define DS_DA_IMPLEMENTATION
#include "ds.h"

/// LEXER

#define SYMBOL_COMMA ','
#define SYMBOL_MINUS '-'
#define SYMBOL_GT '>'
#define SYMBOL_LT '>'
#define SYMBOL_EQ '='
#define SYMBOL_LPAREN '('
#define SYMBOL_RPAREN ')'
#define SYMBOL_NEWLINE '\n'
#define SYMBOL_QUOTE '"'
#define SYMBOL_BACKSLASH '\\'
#define SYMBOL_NULL '\0'

#define isname(c) ((isalnum((c)) || ispunct((c))) && (c) != SYMBOL_LPAREN && (c) != SYMBOL_RPAREN && (c) != SYMBOL_COMMA)

#define SLICE_FUNC DS_STRING_SLICE("func")
#define SLICE_IN DS_STRING_SLICE("in")
#define SLICE_END DS_STRING_SLICE("end")
#define SLICE_DATA DS_STRING_SLICE("data")
#define SLICE_TRUE DS_STRING_SLICE("true")
#define SLICE_FALSE DS_STRING_SLICE("false")
#define SLICE_IF DS_STRING_SLICE("if")
#define SLICE_ELSE DS_STRING_SLICE("else")
#define SLICE_FI DS_STRING_SLICE("fi")
#define SLICE_IMPORT DS_STRING_SLICE("@import")

typedef enum {
    STACK_ILLEGAL_NO_ERROR,
    STACK_ILLEGAL_INVALID,
    STACK_ILLEGAL_STRING_NULL,
    STACK_ILLEGAL_STRING_NEWLINE,
    STACK_ILLEGAL_STRING_EOF,
} stack_token_error_kind;

static const char *stack_token_error_kind_map(stack_token_error_kind kind) {
    switch (kind) {
    case STACK_ILLEGAL_NO_ERROR: return "NO_ERROR";
    case STACK_ILLEGAL_INVALID: return "Invalid character: ";
    case STACK_ILLEGAL_STRING_NULL: return "String contains null character";
    case STACK_ILLEGAL_STRING_NEWLINE: return "String contains new line character";
    case STACK_ILLEGAL_STRING_EOF: return "Unterminated string";
      break;
    }
}

typedef enum {
    STACK_TOKEN_IMPORT,
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
    STACK_TOKEN_STRING,
    STACK_TOKEN_IF,
    STACK_TOKEN_ELSE,
    STACK_TOKEN_FI,
    STACK_TOKEN_EOF,
    STACK_TOKEN_ILLEGAL,
} stack_token_kind;

static const char* stack_token_kind_map(stack_token_kind kind) {
    switch (kind) {
    case STACK_TOKEN_IMPORT: return "IMPORT";
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
    case STACK_TOKEN_STRING: return "STRING";
    case STACK_TOKEN_IF: return "IF";
    case STACK_TOKEN_ELSE: return "ELSE";
    case STACK_TOKEN_FI: return "FI";
    case STACK_TOKEN_EOF: return "<EOF>";
    case STACK_TOKEN_ILLEGAL: return "ILLEGAL";
    }
}

typedef struct {
    stack_token_kind kind;
    ds_string_slice value;
    unsigned int pos;
    stack_token_error_kind error;
} stack_token;

#define STACK_TOKEN(k, v, p) (stack_token){.kind = (k), .value = (v), .pos = (p), .error = STACK_ILLEGAL_NO_ERROR}
#define STACK_TOKEN_ERROR(v, p, e) (stack_token){.kind = STACK_TOKEN_ILLEGAL, .value = (v), .pos = (p), .error = (e)}

void stack_token_free(stack_token *token) {
    ds_string_slice_free(&token->value);

    token->pos = 0;
}

typedef struct {
    char *buffer;
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

int stack_lexer_init(stack_lexer *lexer, char *buffer, unsigned int buffer_len) {
    lexer->buffer = buffer;
    lexer->buffer_len = buffer_len;
    lexer->pos = 0;
    lexer->read_pos = 0;
    lexer->ch = 0;

    stack_lexer_read(lexer);

    return 0;
}

void stack_lexer_free(stack_lexer *lexer) {
    lexer->buffer = NULL;
    lexer->buffer_len = 0;
    lexer->pos = 0;
    lexer->read_pos = 0;
    lexer->ch = 0;
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
    } else if (lexer->ch == SYMBOL_QUOTE) {
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 0 };

        slice.len += 1;
        stack_lexer_read(lexer);

        while (lexer->ch != SYMBOL_QUOTE) {
            if (lexer->ch == EOF) {
                stack_lexer_skip_until_newline(lexer);
                return STACK_TOKEN_ERROR((ds_string_slice){0}, position, STACK_ILLEGAL_STRING_EOF);
            }

            if (lexer->ch == SYMBOL_NULL) {
                stack_lexer_skip_until_newline(lexer);
                return STACK_TOKEN_ERROR((ds_string_slice){0}, position, STACK_ILLEGAL_STRING_NULL);
            }

            if (lexer->ch == SYMBOL_NEWLINE) {
                stack_lexer_skip_until_newline(lexer);
                return STACK_TOKEN_ERROR((ds_string_slice){0}, position, STACK_ILLEGAL_STRING_NEWLINE);
            }

            if (lexer->ch == SYMBOL_BACKSLASH) {
                slice.len += 1;
                stack_lexer_read(lexer);
            }

            slice.len += 1;
            stack_lexer_read(lexer);
        }

        slice.len += 1;
        stack_lexer_read(lexer);

        return STACK_TOKEN(STACK_TOKEN_STRING, slice, position);
    } else if (isname(lexer->ch)) {
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 0 };

        while (isname(lexer->ch)) {
            slice.len += 1;
            stack_lexer_read(lexer);
        }

        if (ds_string_slice_equals(&slice, &SLICE_IMPORT)) {
            return STACK_TOKEN(STACK_TOKEN_IMPORT, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_FUNC)) {
            return STACK_TOKEN(STACK_TOKEN_FUNC, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_IN)) {
            return STACK_TOKEN(STACK_TOKEN_IN, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_END)) {
            return STACK_TOKEN(STACK_TOKEN_END, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_DATA)) {
            return STACK_TOKEN(STACK_TOKEN_DATA, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_TRUE) || ds_string_slice_equals(&slice, &SLICE_FALSE)) {
            return STACK_TOKEN(STACK_TOKEN_BOOLEAN, slice, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_IF)) {
            return STACK_TOKEN(STACK_TOKEN_IF, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_ELSE)) {
            return STACK_TOKEN(STACK_TOKEN_ELSE, (ds_string_slice){0}, position);
        } else if (ds_string_slice_equals(&slice, &SLICE_FI)) {
            return STACK_TOKEN(STACK_TOKEN_FI, (ds_string_slice){0}, position);
        } else {
            return STACK_TOKEN(STACK_TOKEN_NAME, slice, position);
        }
    } else {
        char *value = NULL;
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 1 };

        stack_lexer_read(lexer);

        return STACK_TOKEN_ERROR(slice, position, STACK_ILLEGAL_INVALID);
    }
}

void stack_lexer_dump(stack_lexer *lexer) {
    stack_token token = {0};
    stack_token_kind kind;

    do {
        token = stack_lexer_next(lexer);
        kind = token.kind;

        fprintf(stdout, "%s", stack_token_kind_map(token.kind));

        if (token.value.str != NULL) {
            fprintf(stdout, "(%.*s)", token.value.len, token.value.str);
        }
        fprintf(stdout, "\n");

        stack_token_free(&token);
    } while (kind != STACK_TOKEN_EOF);
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

/// PARSER

typedef struct {
    stack_lexer *lexer;
    stack_token tok;
    stack_token next_tok;

    bool failed;

    char *filename;
} stack_parser;

static stack_token stack_parser_peek(stack_parser *parser) {
    return parser->next_tok;
}

static stack_token stack_parser_read(stack_parser *parser) {
    parser->tok = stack_parser_peek(parser);

    parser->next_tok = stack_lexer_next(parser->lexer);

    return parser->tok;
}

int stack_parser_init(stack_parser *parser, stack_lexer *lexer, char *filename) {
    parser->lexer = lexer;
    parser->tok = (stack_token){0};
    parser->next_tok = (stack_token){0};
    parser->filename = filename;

    parser->failed = false;

    stack_parser_read(parser);

    return 0;
}

static void stack_parser_show_errorf(stack_parser *parser, const char *format, ...) {
    unsigned int line = 0;
    unsigned int col = 0;
    stack_token tok = parser->tok;
    stack_lexer_pos_to_lc(parser->lexer, tok.pos, &line, &col);

    if (parser->filename != NULL) {
        fprintf(stderr, "%s", parser->filename);
    }

    fprintf(stderr, ":%d:%d, ", line, col);
    if (tok.kind == STACK_TOKEN_ILLEGAL) {
        fprintf(stderr, "Lexical error: %s", stack_token_error_kind_map(tok.error));

        if (tok.value.str != NULL) {
            fprintf(stderr, ": (%.*s)", tok.value.len, tok.value.str);
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
    parser->lexer = NULL;

    parser->lexer = NULL;
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
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }
    field->type = STACK_AST_NODE(token.value, parser, token.pos);

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }
    field->name = STACK_AST_NODE(token.value, parser, token.pos);

    return_defer(0);

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
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }
    data->name = STACK_AST_NODE(token.value, parser, token.pos);

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_LPAREN) {
        stack_parser_show_expected(parser, STACK_TOKEN_LPAREN, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }

    token = stack_parser_peek(parser);
    if (token.kind == STACK_TOKEN_RPAREN) {
        stack_parser_read(parser);
        return_defer(0);
    }

    do {
        stack_ast_data_field field = {0};
        if (stack_parser_parse_data_field(parser, &field) != 0) {
            DS_PANIC("TODO: RECOVER FROM ERROR");
        }
        ds_dynamic_array_append(&data->fields, &field);

        token = stack_parser_read(parser);
        if (token.kind == STACK_TOKEN_RPAREN) {
            break;
        } else if (token.kind == STACK_TOKEN_COMMA) {
            continue;
        } else {
            stack_parser_show_expected_2(parser, STACK_TOKEN_COMMA, STACK_TOKEN_RPAREN, token.kind);
            DS_PANIC("TODO: RECOVER FROM ERROR");
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
    STACK_AST_EXPR_STRING,
    STACK_AST_EXPR_NAME,
    STACK_AST_EXPR_COND,
} stack_ast_expr_kind;

typedef struct {
    ds_dynamic_array if_; //stack_ast_expr
    ds_dynamic_array else_; // stack_ast_expr
} stack_ast_cond;

typedef struct {
    stack_ast_expr_kind kind;
    union {
        stack_ast_node number;
        stack_ast_node boolean;
        stack_ast_node string;
        stack_ast_node name;
        stack_ast_cond cond;
    };
} stack_ast_expr;

#define STACK_AST_EXPR_NUMBER(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_NUMBER, .number = (node)}
#define STACK_AST_EXPR_BOOLEAN(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_BOOLEAN, .boolean = (node)}
#define STACK_AST_EXPR_STRING(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_STRING, .string = (node)}
#define STACK_AST_EXPR_NAME(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_NAME, .name = (node)}
#define STACK_AST_EXPR_COND(node) (stack_ast_expr){ .kind = STACK_AST_EXPR_COND, .cond = (node)}

static int stack_parser_parse_expr(stack_parser *parser, stack_ast_expr *expr);
static void stack_ast_expr_free(stack_ast_expr *expr);
static void stack_ast_expr_dump(stack_ast_expr *prog, FILE* stdout, unsigned int indent);

static int stack_parser_parse_cond(stack_parser *parser, stack_ast_cond *cond) {
    stack_token token = {0};
    int result = 0;

    ds_dynamic_array_init(&cond->if_, sizeof(stack_ast_expr));
    ds_dynamic_array_init(&cond->else_, sizeof(stack_ast_expr));

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_IF) {
        stack_parser_show_expected(parser, STACK_TOKEN_IF, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }

    do {
        token = stack_parser_peek(parser);
        if (token.kind == STACK_TOKEN_FI) {
            stack_parser_read(parser);
            return_defer(0);
        }
        if (token.kind == STACK_TOKEN_ELSE) {
            stack_parser_read(parser);
            break;
        }

        stack_ast_expr expr = {0};
        if (stack_parser_parse_expr(parser, &expr) != 0) {
            DS_PANIC("TODO: RECOVER FROM ERROR");
        }
        DS_EXPECT(ds_dynamic_array_append(&cond->if_, &expr), DS_ERROR_OOM);
    } while (true);

    do {
        token = stack_parser_peek(parser);
        if (token.kind == STACK_TOKEN_FI) {
            stack_parser_read(parser);
            return_defer(0);
        }

        stack_ast_expr expr = {0};
        if (stack_parser_parse_expr(parser, &expr) != 0) {
            DS_PANIC("TODO: RECOVER FROM ERROR");
        }
        DS_EXPECT(ds_dynamic_array_append(&cond->else_, &expr), DS_ERROR_OOM);
    } while (true);

    stack_parser_show_expected(parser, STACK_TOKEN_FI, token.kind);
    DS_PANIC("TODO: RECOVER FROM ERROR");

defer:
    return result;
}

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
    case STACK_TOKEN_STRING:
        stack_parser_read(parser);
        *expr = STACK_AST_EXPR_STRING(STACK_AST_NODE(token.value, parser, token.pos));
        return_defer(0);
    case STACK_TOKEN_IF: {
        stack_ast_cond cond = {0};
        if (stack_parser_parse_cond(parser, &cond) != 0) {
            DS_PANIC("TODO: RECOVER FROM ERROR");
        }
        *expr = STACK_AST_EXPR_COND(cond);
        return_defer(0);
    }
    default:
        stack_parser_read(parser);
        return_defer(1);
    }

defer:
    return result;
}

#define INDENT_INCREMENT 4

static void stack_ast_cond_dump(stack_ast_cond *cond, FILE* stdout, unsigned int indent) {
    fprintf(stdout, "%*sIF\n", indent, "");
    for (unsigned int i = 0; i < cond->if_.count; i++) {
        stack_ast_expr expr = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&cond->if_, i, &expr));
        stack_ast_expr_dump(&expr, stdout, indent + INDENT_INCREMENT);
    }
    fprintf(stdout, "%*sELSE\n", indent, "");
    for (unsigned int i = 0; i < cond->else_.count; i++) {
        stack_ast_expr expr = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&cond->else_, i, &expr));
        stack_ast_expr_dump(&expr, stdout, indent + INDENT_INCREMENT);
    }
    fprintf(stdout, "%*sFI\n", indent, "");
}

static void stack_ast_expr_dump(stack_ast_expr *expr, FILE* stdout, unsigned int indent) {
    switch (expr->kind) {
    case STACK_AST_EXPR_NUMBER: {
        ds_string_slice slice = {0};
        slice = expr->number.value;
        fprintf(stdout, "%*s%.*s\n", indent, "", slice.len, slice.str);
        break;
    }
    case STACK_AST_EXPR_BOOLEAN: {
        ds_string_slice slice = {0};
        slice = expr->boolean.value;
        fprintf(stdout, "%*s%.*s\n", indent, "", slice.len, slice.str);
        break;
    }
    case STACK_AST_EXPR_STRING: {
        ds_string_slice slice = {0};
        slice = expr->string.value;
        fprintf(stdout, "%*s%.*s\n", indent, "", slice.len, slice.str);
        break;
    }
    case STACK_AST_EXPR_NAME: {
        ds_string_slice slice = {0};
        slice = expr->name.value;
        fprintf(stdout, "%*s%.*s\n", indent, "", slice.len, slice.str);
        break;
    }
    case STACK_AST_EXPR_COND:
        stack_ast_cond_dump(&expr->cond, stdout, indent);
        break;
    }
}

static void stack_ast_cond_free(stack_ast_cond *cond) {
    for (unsigned int i = 0; i < cond->if_.count; i++) {
        stack_ast_expr *expr = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&cond->if_, i, (void **)&expr));
        stack_ast_expr_free(expr);
    }
    for (unsigned int i = 0; i < cond->else_.count; i++) {
        stack_ast_expr *expr = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&cond->else_, i, (void **)&expr));
        stack_ast_expr_free(expr);
    }
    ds_dynamic_array_free(&cond->if_);
    ds_dynamic_array_free(&cond->else_);
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
    case STACK_AST_EXPR_STRING:
        stack_ast_node_free(&expr->string);
        break;
    case STACK_AST_EXPR_COND:
        stack_ast_cond_free(&expr->cond);
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
        DS_PANIC("TODO: RECOVER FROM ERROR");
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
            DS_PANIC("TODO: RECOVER FROM ERROR");
        }
        ds_dynamic_array_append(nodes, &STACK_AST_NODE(token.value, parser, token.pos));

        token = stack_parser_read(parser);
        if (token.kind == STACK_TOKEN_RPAREN) {
            break;
        } else if (token.kind == STACK_TOKEN_COMMA) {
            continue;
        } else {
            stack_parser_show_expected_2(parser, STACK_TOKEN_COMMA, STACK_TOKEN_RPAREN, token.kind);
            DS_PANIC("TODO: RECOVER FROM ERROR");
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
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }
    func->name = STACK_AST_NODE(token.value, parser, token.pos);

    stack_parser_parse_func_nodes(parser, &func->args);
    stack_parser_parse_func_nodes(parser, &func->rets);

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_IN) {
        stack_parser_show_expected(parser, STACK_TOKEN_IN, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }

    do {
        token = stack_parser_peek(parser);
        if (token.kind == STACK_TOKEN_END) {
            stack_parser_read(parser);
            return_defer(0);
        }

        stack_ast_expr expr = {0};
        if (stack_parser_parse_expr(parser, &expr) != 0) {
            if (token.kind == STACK_TOKEN_ILLEGAL) {
                stack_parser_show_errorf(parser, "");
                stack_parser_read(parser);
                parser->failed = true;
                continue;
            } else {
                stack_parser_show_expected(parser, STACK_TOKEN_END, token.kind);
                DS_PANIC("TODO: RECOVER FROM ERROR");
            }
        }
        ds_dynamic_array_append(&func->body, &expr);
    } while (true);

    if (parser->failed) {
        return_defer(1);
    }

defer:
    return result;
}

static void stack_ast_func_free(stack_ast_func *func) {
    stack_ast_node_free(&func->name);
    for (unsigned int i = 0; i < func->args.count; i++) {
        stack_ast_node *arg = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&func->args, i, (void **)&arg));
        stack_ast_node_free(arg);
    }
    ds_dynamic_array_free(&func->args);

    for (unsigned int i = 0; i < func->rets.count; i++) {
        stack_ast_node *ret = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&func->rets, i, (void **)&ret));
        stack_ast_node_free(ret);
    }
    ds_dynamic_array_free(&func->rets);

    for (unsigned int i = 0; i < func->body.count; i++) {
        stack_ast_expr *expr = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&func->body, i, (void **)&expr));
        stack_ast_expr_free(expr);
    }
    ds_dynamic_array_free(&func->body);
}

typedef struct {
    stack_ast_node name;
} stack_ast_import;

static int stack_parser_parse_import(stack_parser *parser, stack_ast_import *import) {
    stack_token token = {0};
    int result = 0;

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_IMPORT) {
        stack_parser_show_expected(parser, STACK_TOKEN_IMPORT, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        stack_parser_show_expected(parser, STACK_TOKEN_NAME, token.kind);
        DS_PANIC("TODO: RECOVER FROM ERROR");
    }
    import->name = STACK_AST_NODE(token.value, parser, token.pos);

    return_defer(0);

defer:
    return result;
}

static void stack_ast_import_free(stack_ast_import *import) {
    stack_ast_node_free(&import->name);
}

typedef struct {
    ds_dynamic_array datas; // stack_ast_data
    ds_dynamic_array funcs; // stack_ast_func
    ds_dynamic_array imports; // stack_ast_import
} stack_ast_prog;

void stack_ast_init(stack_ast_prog *prog) {
    ds_dynamic_array_init(&prog->datas, sizeof(stack_ast_data));
    ds_dynamic_array_init(&prog->funcs, sizeof(stack_ast_func));
    ds_dynamic_array_init(&prog->imports, sizeof(stack_ast_import));
}

int stack_parser_parse(stack_parser *parser, stack_ast_prog *prog) {
    int result = 0;

    while (true) {
        stack_token token = stack_parser_peek(parser);
        if (token.kind == STACK_TOKEN_DATA) {
            stack_ast_data data = {0};
            if (stack_parser_parse_data(parser, &data) != 0) {
                continue;
            }
            DS_EXPECT(ds_dynamic_array_append(&prog->datas, &data), DS_ERROR_OOM);
        } else if (token.kind == STACK_TOKEN_FUNC) {
            stack_ast_func func = {0};
            if (stack_parser_parse_func(parser, &func) != 0) {
                continue;
            }
            DS_EXPECT(ds_dynamic_array_append(&prog->funcs, &func), DS_ERROR_OOM);
        } else if (token.kind == STACK_TOKEN_IMPORT) {
            stack_ast_import import = {0};
            if (stack_parser_parse_import(parser, &import) != 0) {
                continue;
            }
            DS_EXPECT(ds_dynamic_array_append(&prog->imports, &import), DS_ERROR_OOM);
        } else if (token.kind == STACK_TOKEN_EOF) {
            break;
        } else {
            stack_parser_show_expected_3(parser, STACK_TOKEN_DATA, STACK_TOKEN_FUNC, STACK_TOKEN_EOF, token.kind);
            DS_PANIC("TODO: RECOVER FROM ERROR");
        }
    }

    if (parser->failed) {
        return_defer(1);
    }

defer:
    return result;
}

void stack_ast_dump(stack_ast_prog *prog, FILE* stdout) {
    const int indent = INDENT_INCREMENT;

    for (unsigned int i = 0; i < prog->imports.count; i++) {
        stack_ast_import import = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&prog->imports, i, &import));

        fprintf(stdout, "@import %.*s\n", import.name.value.len, import.name.value.str);
    }
    if (prog->imports.count > 0) fprintf(stdout, "\n");

    for (unsigned int i = 0; i < prog->datas.count; i++) {
        stack_ast_data data = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&prog->datas, i, &data));

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
        DS_UNREACHABLE(ds_dynamic_array_get(&prog->funcs, i, &func));

        fprintf(stdout, "func %.*s\n", func.name.value.len, func.name.value.str);
        for (unsigned int j = 0; j < func.args.count; j++) {
            stack_ast_node arg = {0};
            DS_UNREACHABLE(ds_dynamic_array_get(&func.args, j, &arg));

            fprintf(stdout, "%*sarg%d: %.*s\n", indent, "", j, arg.value.len, arg.value.str);
        }
        for (unsigned int j = 0; j < func.rets.count; j++) {
            stack_ast_node ret = {0};
            DS_UNREACHABLE(ds_dynamic_array_get(&func.rets, j, &ret));

            fprintf(stdout, "%*sret%d: %.*s\n", indent, "", j, ret.value.len, ret.value.str);
        }

        fprintf(stdout, "%*sbody:\n", indent, "");
        for (unsigned int j = 0; j < func.body.count; j++) {
            stack_ast_expr expr = {0};
            DS_UNREACHABLE(ds_dynamic_array_get(&func.body, j, &expr));

            stack_ast_expr_dump(&expr, stdout, indent + INDENT_INCREMENT);
        }
        fprintf(stdout, "\n");
    }
}

void stack_ast_free(stack_ast_prog *prog) {
    for (unsigned int i = 0; i < prog->datas.count; i++) {
        stack_ast_data *data = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&prog->datas, i, (void **)&data));
        stack_ast_data_free(data);
    }
    ds_dynamic_array_free(&prog->datas);

    for (unsigned int i = 0; i < prog->funcs.count; i++) {
        stack_ast_func *func = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&prog->funcs, i, (void **)&func));
        stack_ast_func_free(func);
    }
    ds_dynamic_array_free(&prog->funcs);

    for (unsigned int i = 0; i < prog->imports.count; i++) {
        stack_ast_import *import = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get_ref(&prog->imports, i, (void **)&import));
        stack_ast_import_free(import);
    }
    ds_dynamic_array_free(&prog->imports);
}

/// PREPROCESSOR

// TODO: remove buffers
typedef struct {
    ds_dynamic_array _lexers; // stack_lexer *
    ds_dynamic_array _parsers; // stack_parser *
    ds_dynamic_array _buffers; // char *
} stack_preprocessor;

void stack_preprocessor_init(stack_preprocessor *preprocessor) {
    ds_dynamic_array_init(&preprocessor->_lexers, sizeof(stack_lexer));
    ds_dynamic_array_init(&preprocessor->_parsers, sizeof(stack_parser));
    ds_dynamic_array_init(&preprocessor->_buffers, sizeof(char *));
}

void stack_preprocessor_free(stack_preprocessor *preprocessor) {
    for (unsigned int i = 0; i < preprocessor->_lexers.count; i++) {
        stack_lexer lexer = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&preprocessor->_lexers, i, &lexer));
        stack_lexer_free(&lexer);
    }
    ds_dynamic_array_free(&preprocessor->_lexers);

    for (unsigned int i = 0; i < preprocessor->_parsers.count; i++) {
        stack_parser parser = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&preprocessor->_parsers, i, &parser));
        stack_parser_free(&parser);
    }
    ds_dynamic_array_free(&preprocessor->_parsers);

    for (unsigned int i = 0; i < preprocessor->_buffers.count; i++) {
        char *buffer = NULL;
        DS_UNREACHABLE(ds_dynamic_array_get(&preprocessor->_buffers, i, &buffer));
        DS_FREE(NULL, buffer);
    }
    ds_dynamic_array_free(&preprocessor->_buffers);
}

int stack_preprocessor_run(stack_preprocessor *preprocessor, stack_ast_prog *prog) {
    ds_string_builder sb = {0};
    unsigned int buffer_len = 0;
    int result = 0;

    // TODO: dependency graph based on `import` and merge all the ASTs
    // TODO: BUILD DEP GRAPH
    // TODO: Make sure no cycle import
    // TODO: memory hadling
    // WIP
    for (unsigned int i = 0; i < prog->imports.count; i++) {
        char *buffer = NULL;
        char *filename = NULL;
        stack_lexer lexer = {0};
        stack_parser parser = {0};
        stack_ast_import import = {0};

        ds_dynamic_array_get(&prog->imports, i, &import);

        ds_string_builder_init(&sb);
        DS_EXPECT(ds_string_builder_append(&sb, "lib/%.*s.sl", import.name.value.len, import.name.value.str), DS_ERROR_OOM);
        DS_EXPECT(ds_string_builder_build(&sb, &filename), DS_ERROR_OOM);
        DS_EXPECT(ds_dynamic_array_append(&preprocessor->_buffers, &filename), DS_ERROR_OOM);

        buffer_len = ds_io_read(filename, &buffer, "r");
        if (buffer_len < 0) {
            DS_LOG_ERROR("Failed to read from file: %s", filename);
            return_defer(1);
        }

        DS_EXPECT(ds_dynamic_array_append(&preprocessor->_buffers, &buffer), DS_ERROR_OOM);

        stack_lexer_init(&lexer, buffer, buffer_len);

        stack_parser_init(&parser, &lexer, filename);
        DS_EXPECT(ds_dynamic_array_append(&preprocessor->_parsers, &parser), DS_ERROR_OOM);

        // TODO: will probably have multiple prog's
        if (stack_parser_parse(&parser, prog) != 0) {
            return_defer(1);
        }

        // TODO: Fix this by using references better
        DS_EXPECT(ds_dynamic_array_append(&preprocessor->_lexers, &lexer), DS_ERROR_OOM);

        ds_string_builder_free(&sb);
    }

defer:
    ds_string_builder_free(&sb);
    return result;
}

/// TYPE CHECKER

#define STACK_DATA_INT "int"
#define STACK_DATA_BOOL "bool"
#define STACK_DATA_STRING "string"

#define STACK_FUNC_MAIN "main"
#define STACK_FUNC_DUP "dup"
#define STACK_FUNC_SWP "swp"
#define STACK_FUNC_ROT "rot"
#define STACK_FUNC_ROTP "rot'"
#define STACK_FUNC_POP "pop"

#define STACK_FUNC_PLUS "+"
#define STACK_FUNC_MINUS "-"
#define STACK_FUNC_STAR "*"
#define STACK_FUNC_DIV "/"
#define STACK_FUNC_MOD "%"
#define STACK_FUNC_GT ">"
#define STACK_FUNC_LT "<"
#define STACK_FUNC_EQ "="
#define STACK_FUNC_OUT "string.out"
#define STACK_FUNC_STRING_LEN "string.len"
#define STACK_FUNC_STRING_CONCAT "string.concat"
#define STACK_FUNC_STRING_SUBSTR "string.substr"

#define STACK_FUNC_MEMORY_ALLOCATE "memory.allocate"
#define STACK_FUNC_MEMORY_STORE "memory.@"
#define STACK_FUNC_MEMORY_DEREF "memory.!!"

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

/// ASSEMBLER

#define STACK_WORD_SZ 8

#define STACK_CONST "stack_const"

#define STACK_CONST_INT STACK_CONST "_" STACK_DATA_INT
#define STACK_CONST_BOOL STACK_CONST "_" STACK_DATA_BOOL
#define STACK_CONST_STRING STACK_CONST "_" STACK_DATA_STRING

typedef enum {
    STACK_CONST_EXPR_INT,
    STACK_CONST_EXPR_BOOL,
    STACK_CONST_EXPR_STRING,
} stack_const_expr_kind;

typedef struct {
    stack_const_expr_kind kind;
    ds_string_slice value;
    bool allocd;
} stack_const_expr;

void stack_const_expr_free(stack_const_expr *expr) {
    if (expr->allocd && expr->value.str != NULL) DS_FREE(expr->value.allocator, expr->value.str);

    ds_string_slice_free(&expr->value);
}

#define STACK_CONST_EXPR(k, v) (stack_const_expr){.kind = (k), .value = (v), .allocd = false}
#define STACK_CONST_EXPR_ALLOCD(k, v) (stack_const_expr){.kind = (k), .value = (v), .allocd = true}

typedef struct {
    ds_string_slice name;
    bool allocd;
} stack_const_func;

void stack_const_func_free(stack_const_func *func) {
    if (func->allocd && func->name.str != NULL) DS_FREE(func->name.allocator, func->name.str);

    ds_string_slice_free(&func->name);
}

#define STACK_CONST_FUNC(v) (stack_const_func){.name = (v), .allocd = false}
#define STACK_CONST_FUNC_ALLOCD(v) (stack_const_func){.name = (v), .allocd = true}

typedef struct {
    ds_dynamic_array func_map; // stack_const_func
    ds_dynamic_array constants; // stack_const_expr
    unsigned int if_counter;

    FILE *stdout;
} stack_assembler;

void stack_assembler_init(stack_assembler *assembler, FILE *stdout) {
    ds_dynamic_array_init(&assembler->func_map, sizeof(stack_const_func));
    ds_dynamic_array_init(&assembler->constants, sizeof(stack_const_expr));
    assembler->if_counter = 0;

    assembler->stdout = stdout;
}

void stack_assembler_free(stack_assembler *assembler) {
    for (unsigned int i = 0; i < assembler->func_map.count; i++) {
        stack_const_func func = {0};
        ds_dynamic_array_get(&assembler->func_map, i, &func);
        stack_const_func_free(&func);
    }
    ds_dynamic_array_free(&assembler->func_map);

    for (unsigned int i = 0; i < assembler->constants.count; i++) {
        stack_const_expr expr = {0};
        ds_dynamic_array_get(&assembler->constants, i, &expr);
        stack_const_expr_free(&expr);
    }
    ds_dynamic_array_free(&assembler->constants);
    assembler->if_counter = 0;

    assembler->stdout = NULL;
}

static unsigned long stack_assembler_func_map(stack_assembler *assembler, stack_const_func *func, bool *found) {
    for (unsigned int i = 0; i < assembler->func_map.count; i++) {
        stack_const_func item = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&assembler->func_map, i, &item));
        if (ds_string_slice_equals(&func->name, &item.name)) {
            if (found != NULL) *found = true;
            return i;
        }
    }

    DS_EXPECT(ds_dynamic_array_append(&assembler->func_map, func), DS_ERROR_OOM);
    if (found != NULL) *found = false;
    return assembler->func_map.count - 1;
}

static unsigned int stack_assembler_constants_map(stack_assembler *assembler, stack_const_expr *expr, bool *found) {
    for (unsigned int i = 0; i < assembler->constants.count; i++) {
        stack_const_expr item = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&assembler->constants, i, &item));
        if (expr->kind == item.kind && ds_string_slice_equals(&expr->value, &item.value)) {
            if (found != NULL) *found = true;
            return i;
        }
    }

    DS_EXPECT(ds_dynamic_array_append(&assembler->constants, expr), DS_ERROR_OOM);
    if (found != NULL) *found = false;
    return assembler->constants.count - 1;
}

// TODO: maybe refactor big chunks of EMIT's into things like `EMIT_POP`
#define EMIT(format, ...) fprintf(assembler->stdout, format "\n", ##__VA_ARGS__)
#define EMIT_N(format, ...) fprintf(assembler->stdout, format, ##__VA_ARGS__)

static void stack_assembler_emit_entry(stack_assembler *assembler) {
    EMIT("section '.data' writeable");
    EMIT("");
    EMIT("; Define some constants");
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
    EMIT("    call   func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_MAIN)), NULL), STACK_FUNC_MAIN);
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
    EMIT("    add     qword [stack_pos], %d", STACK_WORD_SZ);
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
    EMIT("    mov     rax, qword [rax - %d]", STACK_WORD_SZ);
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
    EMIT("    mov     rax, qword [rax - %d]", STACK_WORD_SZ);
    EMIT("    sub     qword [stack_pos], %d", STACK_WORD_SZ);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_DUP)), NULL), STACK_FUNC_DUP);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_SWP)), NULL), STACK_FUNC_SWP);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_ROT)), NULL), STACK_FUNC_ROT);
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
    // ROT'
    EMIT(";");
    EMIT(";");
    EMIT("; rot'");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_ROTP)), NULL), STACK_FUNC_ROTP);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 32                    ; allocate 4 local variables");
    EMIT("");
    EMIT("    ; (A B C) -> (C A B)");
    EMIT("");
    EMIT("    ; t0 <- C");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; t1 <- B");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_1], rax");
    EMIT("");
    EMIT("    ; t2 <- A");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_2], rax");
    EMIT("");
    EMIT("    ; push C");
    EMIT("    mov     rdi, [rbp - loc_0]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    ; push A");
    EMIT("    mov     rdi, [rbp - loc_2]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    ; push B");
    EMIT("    mov     rdi, [rbp - loc_1]");
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_POP)), NULL), STACK_FUNC_POP);
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
    EMIT("func.%s:", STACK_DATA_INT);
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
    // PLUS
    EMIT(";");
    EMIT(";");
    EMIT("; plus");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_PLUS)), NULL), STACK_FUNC_PLUS);
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
    EMIT("    call    func.%s", STACK_DATA_INT);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // SUB
    EMIT(";");
    EMIT(";");
    EMIT("; sub");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_MINUS)), NULL), STACK_FUNC_MINUS);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    push    rax");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rdi, qword [rax]");
    EMIT("    pop     rax");
    EMIT("");
    EMIT("    sub     rdi, rax");
    EMIT("    call    func.%s", STACK_DATA_INT);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_STAR)), NULL), STACK_FUNC_STAR);
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
    EMIT("    call    func.%s", STACK_DATA_INT);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // DIV
    EMIT(";");
    EMIT(";");
    EMIT("; div");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_DIV)), NULL), STACK_FUNC_DIV);
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
    EMIT("    cqo");
    EMIT("    idiv    rdi");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%s", STACK_DATA_INT);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // MOD
    EMIT(";");
    EMIT(";");
    EMIT("; mod");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_MOD)), NULL), STACK_FUNC_MOD);
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
    EMIT("    cqo");
    EMIT("    idiv    rdi");
    EMIT("    mov     rdi, rdx");
    EMIT("    call    func.%s", STACK_DATA_INT);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_GT)), NULL), STACK_FUNC_GT);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    push    rax");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rdi, qword [rax]");
    EMIT("    pop     rax");
    EMIT("");
    EMIT("    cmp     rdi, rax");
    EMIT("    setg    al");
    EMIT("    and     al, 1");
    EMIT("    movzx   rax, al");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%s", STACK_DATA_BOOL);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_LT)), NULL), STACK_FUNC_LT);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    push    rax");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rdi, qword [rax]");
    EMIT("    pop     rax");
    EMIT("");
    EMIT("    cmp     rdi, rax");
    EMIT("    setl    al");
    EMIT("    and     al, 1");
    EMIT("    movzx   rax, al");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%s", STACK_DATA_BOOL);
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
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_EQ)), NULL), STACK_FUNC_EQ);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    push    rax");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rdi, qword [rax]");
    EMIT("    pop     rax");
    EMIT("");
    EMIT("    cmp     rdi, rax");
    EMIT("    sete    al");
    EMIT("    and     al, 1");
    EMIT("    movzx   rax, al");
    EMIT("    mov     rdi, rax");
    EMIT("    call    func.%s", STACK_DATA_BOOL);
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
    EMIT("func.%s:", STACK_DATA_BOOL);
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
    // STRING OUT
    EMIT(";");
    EMIT(";");
    EMIT("; out");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_OUT)), NULL), STACK_FUNC_OUT);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    ; t0 <- string");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; rdx <- t0.len");
    EMIT("    mov     rdx, [rbp - loc_0]");
    EMIT("    mov     rdx, [rdx]");
    EMIT("");
    EMIT("    ; rsi <- t0.str");
    EMIT("    mov     rsi, [rbp - loc_0]");
    EMIT("    lea     rsi, [rsi + 8]");
    EMIT("");
    EMIT("    mov     rdi, 1");
    EMIT("    mov     rax, 1");
    EMIT("    syscall");
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // STRING LENGTH
    EMIT(";");
    EMIT(";");
    EMIT("; string.len");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_STRING_LEN)), NULL), STACK_FUNC_STRING_LEN);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rdi, [rax]");
    EMIT("    call    func.%s", STACK_DATA_INT);
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // STRING CONCAT
    EMIT(";");
    EMIT(";");
    EMIT("; string.concat");
    EMIT(";");
    EMIT(";   Returns a the concatenation of arg1 and arg2");
    EMIT(";");
    EMIT(";   INPUT: (string, string)");
    EMIT(";   OUTPUT: (string)");
    EMIT(";");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_STRING_CONCAT)), NULL), STACK_FUNC_STRING_CONCAT);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 40                    ; allocate 5 local variables");
    EMIT("");
    EMIT("    ; t0 <- s1.len");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_SWP)), NULL), STACK_FUNC_SWP);
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_DUP)), NULL), STACK_FUNC_DUP);
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_STRING_LEN)), NULL), STACK_FUNC_STRING_LEN);
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, [rax]");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_SWP)), NULL), STACK_FUNC_SWP);
    EMIT("");
    EMIT("    ; t1 <- s2.len");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_DUP)), NULL), STACK_FUNC_DUP);
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_STRING_LEN)), NULL), STACK_FUNC_STRING_LEN);
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, [rax]");
    EMIT("    mov     qword [rbp - loc_1], rax");
    EMIT("");
    EMIT("    ; t2 <- t0 + t1");
    EMIT("    mov     rax, qword [rbp - loc_0]");
    EMIT("    add     rax, qword [rbp - loc_1]");
    EMIT("    mov     qword [rbp - loc_2], rax");
    EMIT("");
    EMIT("    ; t3 <- (t2 + 7) >> 3 << 3 + 8");
    EMIT("    mov     rax, qword [rbp - loc_2]");
    EMIT("    add     rax, 7");
    EMIT("    shr     rax, 3");
    EMIT("    shl     rax, 3");
    EMIT("    add     rax, 8");
    EMIT("    mov     qword [rbp - loc_3], rax");
    EMIT("");
    EMIT("    ; t4 <- allocate(t3)");
    EMIT("    mov     rdi, qword [rbp - loc_3]");
    EMIT("    call    allocate");
    EMIT("    mov     qword [rbp - loc_4], rax");
    EMIT("");
    EMIT("    ; t4.len <- t2");
    EMIT("    mov     rax, qword [rbp - loc_4]");
    EMIT("    mov     rdi, qword [rbp - loc_2]");
    EMIT("    mov     [rax], rdi");
    EMIT("");
    EMIT("    ; rdi = t4.str, rsi = s1.str, rdx = t0");
    EMIT("    call    func.%lu ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_SWP)), NULL), STACK_FUNC_SWP);
    EMIT("    call    stack_pop");
    EMIT("    lea     rsi, [rax + 8]");
    EMIT("    mov     rax, qword [rbp - loc_4]");
    EMIT("    lea     rdi, [rax + 8]");
    EMIT("    mov     rdx, qword [rbp - loc_0]");
    EMIT("    call    memcpy");
    EMIT("");
    EMIT("    ; rdi = t4.str + t0, rsi = s2.str, rdx = t1");
    EMIT("    call    stack_pop");
    EMIT("    lea     rsi, [rax + 8]");
    EMIT("    mov     rax, qword [rbp - loc_4]");
    EMIT("    lea     rdi, [rax + 8]");
    EMIT("    add     rdi, qword [rbp - loc_0]");
    EMIT("    mov     rdx, qword [rbp - loc_1]");
    EMIT("    call    memcpy");
    EMIT("");
    EMIT("    ; push t4");
    EMIT("    mov     rdi, [rbp - loc_4]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 40                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // STRING SUBSTR
    EMIT(";");
    EMIT(";");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_STRING_SUBSTR)), NULL), STACK_FUNC_STRING_SUBSTR);
    EMIT(";");
    EMIT(";   Returns the substring starting at int1 and having length int2 for string1");
    EMIT(";");
    EMIT(";	INPUT: (string, int, int)");
    EMIT(";	OUTPUT:	(string)");
    EMIT(";");
    EMIT("String.substr:");
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 40                    ; allocate 5 local variables");
    EMIT("");
    EMIT("    ; t0 <- L");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, [rax]");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; t1 <- (t0 + 7) >> 3 << 3 + 8");
    EMIT("    mov     rax, qword [rbp - loc_0]");
    EMIT("    add     rax, 7");
    EMIT("    shr     rax, 3");
    EMIT("    shl     rax, 3");
    EMIT("    add     rax, 8");
    EMIT("    mov     qword [rbp - loc_1], rax");
    EMIT("");
    EMIT("    ; t2 <- allocate(t1)");
    EMIT("    mov     rdi, qword [rbp - loc_1]");
    EMIT("    call    allocate");
    EMIT("    mov     qword [rbp - loc_2], rax");
    EMIT("");
    EMIT("    ; t2.len <- t0");
    EMIT("    mov     rax, qword [rbp - loc_2]");
    EMIT("    mov     rdi, qword [rbp - loc_0]");
    EMIT("    mov     [rax], rdi");
    EMIT("");
    EMIT("    ; t3 <- i");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, [rax]");
    EMIT("    mov     qword [rbp - loc_3], rax");
    EMIT("");
    EMIT("    ; rdi = t2.str, rsi = s.str + t3, rdx = t0");
    EMIT("    call    stack_pop");
    EMIT("    lea     rsi, [rax + 8]");
    EMIT("    add     rsi, qword [rbp - loc_3]");
    EMIT("    mov     rax, qword [rbp - loc_2]");
    EMIT("    lea     rdi, [rax + 8]");
    EMIT("    mov     rdx, qword [rbp - loc_0]");
    EMIT("    call    memcpy");
    EMIT("");
    EMIT("    ; push t2");
    EMIT("    mov     rdi, [rbp - loc_2]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 40                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // MEMORY ALLOCATE
    EMIT(";");
    EMIT(";");
    EMIT("; memory allocate");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_MEMORY_ALLOCATE)), NULL), STACK_FUNC_MEMORY_ALLOCATE);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 16                    ; allocate 2 local variables");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    shl     rax, 3");
    EMIT("");
    EMIT("    mov     rdi, rax");
    EMIT("    call    allocate");
    EMIT("    mov     rdi, rax");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // MEMORY STORE
    EMIT(";");
    EMIT(";");
    EMIT("; memory store");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_MEMORY_STORE)), NULL), STACK_FUNC_MEMORY_STORE);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 24                    ; allocate 3 local variables");
    EMIT("");
    EMIT("    ; t0 <- a");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    ; t1 <- offset");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    shl     rax, 3");
    EMIT("    mov     qword [rbp - loc_1], rax");
    EMIT("");
    EMIT("    ; t2 <- memory");
    EMIT("    call    stack_pop");
    EMIT("    mov     qword [rbp - loc_2], rax");
    EMIT("");
    EMIT("    ; memory[offset] <- a");
    EMIT("    mov     rax, qword [rbp - loc_2]");
    EMIT("    mov     rdi, qword [rbp - loc_1]");
    EMIT("    add     rax, rdi");
    EMIT("    mov     rdi, qword [rbp - loc_0]");
    EMIT("    mov     qword [rax], rdi");
    EMIT("");
    EMIT("    mov     rdi, qword [rbp - loc_2]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 24                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
    // MEMORY DEREF
    EMIT(";");
    EMIT(";");
    EMIT("; memory store");
    EMIT(";");
    EMIT(";   INPUT: nothing");
    EMIT(";   OUTPUT: nothing");
    EMIT("func.%lu: ; %s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(DS_STRING_SLICE(STACK_FUNC_MEMORY_DEREF)), NULL), STACK_FUNC_MEMORY_DEREF);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");
    EMIT("    sub     rsp, 24                    ; allocate 3 local variables");
    EMIT("");
    EMIT("    ; t0 <- offset");
    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    shl     rax, 3");
    EMIT("    mov     qword [rbp - loc_0], rax");
    EMIT("");
    EMIT("    call    stack_pop");
    EMIT("    mov     rdi, qword [rbp - loc_0]");
    EMIT("    add     rax, rdi");
    EMIT("    mov     rdi, [rax]");
    EMIT("    call    stack_push");
    EMIT("");
    EMIT("    add     rsp, 24                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
}

static void stack_assembler_emit_constants(stack_assembler *assembler) {
    EMIT("section '.data'");
    EMIT("");

    for (unsigned int i = 0; i < assembler->constants.count; i++) {
        stack_const_expr expr = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&assembler->constants, i, &expr));

        switch (expr.kind) {
        case STACK_CONST_EXPR_INT:
            EMIT("%s.%d dq %.*s", STACK_CONST_INT, i, expr.value.len, expr.value.str);
            break;
        case STACK_CONST_EXPR_BOOL:
            EMIT("%s.%d dq %d", STACK_CONST_BOOL, i, ds_string_slice_equals(&expr.value, &SLICE_TRUE) ? 1 : 0);
            break;
        case STACK_CONST_EXPR_STRING: {
            unsigned int count = expr.value.len;

            EMIT("%s.%d dq %d", STACK_CONST_STRING, i, count);
            EMIT_N(" db ");
            if (count == 0) {
                count = 1;
                EMIT_N(" 0");
            }
            for (unsigned int j = 0; j < expr.value.len; j++) {
                EMIT_N("%d", expr.value.str[j]);
                if (j < expr.value.len - 1) EMIT_N(",");
            }
            for (unsigned int j = 0; j < (STACK_WORD_SZ - count % STACK_WORD_SZ); j++) {
                EMIT_N(",0");
            }
            EMIT("");
            break;
        }
        }
    }
}

static void stack_assembler_emit_data(stack_assembler *assembler, stack_ast_data *data) {
    EMIT("func.%lu: ; %.*s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(data->name.value), NULL), data->name.value.len, data->name.value.str);

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
        DS_UNREACHABLE(ds_dynamic_array_get(&data->fields, i, &field));

        EMIT("    ; *t0.%.*s <- pop", field.name.value.len, field.name.value.str);
        EMIT("    call    stack_pop");
        EMIT("    mov     rdi, [rbp - loc_0]");
        EMIT("    mov     qword [rdi + %d], rax", (data->fields.count - i - 1) * STACK_WORD_SZ);
    }

    EMIT("    ; push t0");
    EMIT("    mov     rdi, [rbp - loc_0]");
    EMIT("    call    stack_push");

    EMIT("    add     rsp, 16                    ; deallocate local variables");
    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");

    for (unsigned int i = 0; i < data->fields.count; i++) {
        ds_string_builder sb = {0};
        ds_string_slice slice = {0};
        stack_ast_data_field field = {0};
        bool found = false;

        DS_UNREACHABLE(ds_dynamic_array_get(&data->fields, i, &field));

        ds_string_builder_init(&sb);
        DS_EXPECT(ds_string_builder_append(&sb, "%.*s.%.*s", data->name.value.len, data->name.value.str, field.name.value.len, field.name.value.str), DS_ERROR_OOM);
        ds_string_builder_to_slice(&sb, &slice);
        unsigned long func_label = stack_assembler_func_map(assembler, &STACK_CONST_FUNC_ALLOCD(slice), &found);
        if (found) {
            DS_FREE(slice.allocator, slice.str);
            ds_string_slice_free(&slice);
        }

        EMIT("func.%lu: ; %.*s", func_label, slice.len, slice.str);
        EMIT("    push    rbp                        ; save return address");
        EMIT("    mov     rbp, rsp                   ; set up stack frame");

        EMIT("    call    stack_pop");
        EMIT("    mov     rax, [rax + %d]", i * STACK_WORD_SZ);
        EMIT("    mov     rdi, rax");
        EMIT("    call    stack_push");

        EMIT("    pop     rbp                        ; restore return address");
        EMIT("    ret");
        EMIT("");
    }
}

static void stack_assembler_emit_expr(stack_assembler *assembler, stack_ast_expr *expr);

static void stack_assembler_emit_expr_number(stack_assembler *assembler, stack_ast_node *node) {
    unsigned int constant_label = stack_assembler_constants_map(assembler, &STACK_CONST_EXPR(STACK_CONST_EXPR_INT, node->value), NULL);

    EMIT("    mov     rdi, %s.%d ; %.*s", STACK_CONST_INT, constant_label, node->value.len, node->value.str);
    EMIT("    call    stack_push");
}

static void stack_assembler_emit_expr_boolean(stack_assembler *assembler, stack_ast_node *node) {
    unsigned int constant_label = stack_assembler_constants_map(assembler, &STACK_CONST_EXPR(STACK_CONST_EXPR_BOOL, node->value), NULL);

    EMIT("    mov     rdi, %s.%d ; %.*s", STACK_CONST_BOOL, constant_label, node->value.len, node->value.str);
    EMIT("    call    stack_push");
}

static void stack_assembler_emit_expr_string(stack_assembler *assembler, stack_ast_node *node) {
    ds_string_builder sb = {0};
    ds_string_slice slice = {0};
    char ch = 0;
    bool found = false;

    ds_string_builder_init(&sb);

    for (unsigned int index = 1; index < node->value.len - 1;)  {
        ch = node->value.str[index++];

        if (ch == SYMBOL_BACKSLASH) {
            ch = node->value.str[index++];

            if (ch == 'n') {
                ch = '\n';
            } else if (ch == 't') {
                ch = '\t';
            } else if (ch == 'b') {
                ch = '\b';
            } else if (ch == 'f') {
                ch = '\f';
            }
        }

        DS_EXPECT(ds_string_builder_appendc(&sb, ch), DS_ERROR_OOM);
    }

    ds_string_builder_to_slice(&sb, &slice);
    unsigned int constant_label = stack_assembler_constants_map(assembler, &STACK_CONST_EXPR_ALLOCD(STACK_CONST_EXPR_STRING, slice), &found);
    if (found) {
        DS_FREE(slice.allocator, slice.str);
        ds_string_slice_free(&slice);
    }

    EMIT("    mov     rdi, %s.%d ; %.*s", STACK_CONST_STRING, constant_label, node->value.len, node->value.str);
    EMIT("    call    stack_push");
}

static void stack_assembler_emit_expr_name(stack_assembler *assembler, stack_ast_node *node) {
    EMIT("    call    func.%lu ; %.*s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(node->value), NULL), node->value.len, node->value.str);
}

static void stack_assembler_emit_expr_cond(stack_assembler *assembler, stack_ast_cond *cond) {
    unsigned int if_counter = assembler->if_counter++;

    EMIT("    call    stack_pop");
    EMIT("    mov     rax, qword [rax]");
    EMIT("    test    rax, rax");
    EMIT("    jnz     .if%d", if_counter);
    EMIT(".else%d:", if_counter);
    for (unsigned int i = 0; i < cond->else_.count; i++) {
        stack_ast_expr expr = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&cond->else_, i, &expr));
        stack_assembler_emit_expr(assembler, &expr);
    }
    EMIT("    jmp    .fi%d", if_counter);
    EMIT(".if%d:", if_counter);
    for (unsigned int i = 0; i < cond->if_.count; i++) {
        stack_ast_expr expr = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&cond->if_, i, &expr));
        stack_assembler_emit_expr(assembler, &expr);
    }
    EMIT(".fi%d:", if_counter);
}

static void stack_assembler_emit_expr(stack_assembler *assembler, stack_ast_expr *expr) {
    switch (expr->kind) {
    case STACK_AST_EXPR_NUMBER:
        stack_assembler_emit_expr_number(assembler, &expr->number);
        break;
    case STACK_AST_EXPR_BOOLEAN:
        stack_assembler_emit_expr_boolean(assembler, &expr->boolean);
        break;
    case STACK_AST_EXPR_STRING:
        stack_assembler_emit_expr_string(assembler, &expr->string);
        break;
    case STACK_AST_EXPR_NAME:
        stack_assembler_emit_expr_name(assembler, &expr->name);
        break;
    case STACK_AST_EXPR_COND:
        stack_assembler_emit_expr_cond(assembler, &expr->cond);
        break;
    }
}

static void stack_assembler_emit_func(stack_assembler *assembler, stack_ast_func *func) {
    EMIT("func.%lu: ; %.*s", stack_assembler_func_map(assembler, &STACK_CONST_FUNC(func->name.value), NULL), func->name.value.len, func->name.value.str);
    EMIT("    push    rbp                        ; save return address");
    EMIT("    mov     rbp, rsp                   ; set up stack frame");

    for (unsigned int i = 0; i < func->body.count; i++) {
        stack_ast_expr expr = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&func->body, i, &expr));
        stack_assembler_emit_expr(assembler, &expr);
    }

    EMIT("    pop     rbp                        ; restore return address");
    EMIT("    ret");
    EMIT("");
}

static void stack_assembler_emit_program(stack_assembler *assembler, stack_ast_prog *prog) {
    EMIT("section '.text' executable");
    EMIT("");

    for (unsigned int i = 0; i < prog->datas.count; i++) {
        stack_ast_data data = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&prog->datas, i, &data));
        stack_assembler_emit_data(assembler, &data);
    }

    for (unsigned int i = 0; i < prog->funcs.count; i++) {
        stack_ast_func func = {0};
        DS_UNREACHABLE(ds_dynamic_array_get(&prog->funcs, i, &func));
        stack_assembler_emit_func(assembler, &func);
    }
}

void stack_assembler_emit(stack_assembler *assembler, stack_ast_prog *prog, stack_context *context) {
    EMIT("format ELF64");
    EMIT("");
    stack_assembler_emit_allocator(assembler);
    stack_assembler_emit_entry(assembler);
    stack_assembler_emit_keywords(assembler);
    stack_assembler_emit_program(assembler, prog);
    stack_assembler_emit_constants(assembler);
}

/// MAIN

typedef struct {
    char *filename;
    bool lexer;
    bool parser;
    bool preprocessor;
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
        .short_name = 'P',
        .long_name = "preprocessor",
        .description = "flag to stop on the preprocessor phase",
        .type = ARGUMENT_TYPE_FLAG,
        .required = 0,
    }) != 0) {
        DS_LOG_ERROR("Failed to add argument `preprocessor`");
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
    args->preprocessor = ds_argparse_get_flag(&argparser, "preprocessor");
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
    stack_preprocessor preprocessor = {0};
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
    if (args.lexer) {
        stack_lexer_dump(&lexer);
        return_defer(0);
    }

    stack_parser_init(&parser, &lexer, args.filename);
    stack_ast_init(&prog);
    if (stack_parser_parse(&parser, &prog) != 0) {
        return_defer(1);
    }

    if (args.parser) {
        stack_ast_dump(&prog, stdout);
        return_defer(0);
    }

    stack_preprocessor_init(&preprocessor);
    if (stack_preprocessor_run(&preprocessor, &prog) != 0) {
        return_defer(1);
    }

    if (args.preprocessor) {
        stack_ast_dump(&prog, stdout);
        return_defer(0);
    }

    stack_context_init(&context);
    stack_typechecker_init(&typechecker, args.filename);
    if (stack_typechecker_check(&typechecker, &prog, &context) != 0) {
        return_defer(1);
    }

    if (args.typecheck) {
        return_defer(0);
    }

    stack_assembler_init(&assembler, stdout);
    if (args.assembler) {
        stack_assembler_emit(&assembler, &prog, &context);
        return_defer(0);
    }

defer:
    stack_assembler_free(&assembler);
    stack_typechecker_free(&typechecker);
    stack_preprocessor_free(&preprocessor);
    stack_ast_free(&prog);
    stack_parser_free(&parser);
    stack_lexer_free(&lexer);
    if (buffer != NULL) DS_FREE(NULL, buffer);
    return result;
}
