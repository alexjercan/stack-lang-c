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
    STACK_TOKEN_ARROW,
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
    case STACK_TOKEN_ARROW: return "->";
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
    } else if (lexer->ch == SYMBOL_MINUS && stack_lexer_peek(lexer) == SYMBOL_GT) {
        stack_lexer_read(lexer);
        stack_lexer_read(lexer);
        return (stack_token){.kind = STACK_TOKEN_ARROW, .value = {0}, .pos = position };
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

typedef struct {
    ds_string_slice type;
    ds_string_slice name;
} stack_ast_data_field;

static int stack_parser_parse_data_field(stack_parser *parser, stack_ast_data_field *field) {
    stack_token token = {0};
    int result = 0;

    field->type = (ds_string_slice){0};
    field->name = (ds_string_slice){0};

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        DS_PANIC("PARSER: expected a NAME found %s", stack_token_kind_to_string(token.kind));
        return_defer(1);
    }
    field->type = token.value;

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        DS_PANIC("PARSER: expected a NAME found %s", stack_token_kind_to_string(token.kind));
        return_defer(1);
    }
    field->name = token.value;

defer:
    return result;
}

typedef struct {
    ds_string_slice name;
    ds_dynamic_array fields; // stack_ast_data_field
} stack_ast_data;

static int stack_parser_parse_data(stack_parser *parser, stack_ast_data *data) {
    stack_token token = {0};
    int result = 0;

    data->name = (ds_string_slice){0};
    ds_dynamic_array_init(&data->fields, sizeof(stack_ast_data_field));

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_DATA) {
        DS_PANIC("PARSER: expected `data` found %s", stack_token_kind_to_string(token.kind));
        return_defer(1);
    }

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_NAME) {
        DS_PANIC("PARSER: expected a NAME found %s", stack_token_kind_to_string(token.kind));
        return_defer(1);
    }
    data->name = token.value;

    token = stack_parser_read(parser);
    if (token.kind != STACK_TOKEN_LPAREN) {
        DS_PANIC("PARSER: expected `(` found %s", stack_token_kind_to_string(token.kind));
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
            DS_PANIC("PARSER: expected `,` or `)` found %s", stack_token_kind_to_string(token.kind));
            return_defer(1);
        }
    } while (true);

defer:
    return result;
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
    int result = 0;
    char *buffer = NULL;
    unsigned int buffer_len = 0;
    t_args args = {0};

    if (argparse(argc, argv, &args) != 0) {
        DS_LOG_ERROR("Failed to parse arguments");
        return_defer(1);
    }

    buffer_len = ds_io_read(args.filename, &buffer, "r");
    if (buffer_len < 0) {
        DS_LOG_ERROR("Failed to read from file: %s", (args.filename == NULL) ? "stdin" : args.filename);
        return_defer(1);
    }

    stack_token token = {0};
    stack_lexer lexer = {0};
    stack_parser parser = {0};
    stack_ast_data data = {0};
    stack_lexer_init(&lexer, buffer, buffer_len);
    stack_parser_init(&parser, lexer);

    stack_parser_parse_data(&parser, &data);

    char *name = NULL;

    ds_string_slice_to_owned(&data.name, &name);

    DS_LOG_INFO("%s", name);
    for (unsigned int i = 0; i < data.fields.count; i++) {
        stack_ast_data_field field = {0};
        ds_dynamic_array_get(&data.fields, i, &field);

        char *name = NULL;
        char *type = NULL;

        ds_string_slice_to_owned(&field.type, &type);
        ds_string_slice_to_owned(&field.name, &name);

        DS_LOG_INFO("%s: %s", name, type);
    }

    do {
        token = stack_lexer_next(&lexer);
        char *value = stack_token_to_string(token);
        DS_LOG_INFO("%s", value);
        DS_FREE(NULL, value);
    } while (token.kind != STACK_TOKEN_EOF);

defer:
    if (buffer != NULL) DS_FREE(NULL, buffer);
    return result;
}
