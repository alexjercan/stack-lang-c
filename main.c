#define DS_IO_IMPLEMENTATION
#define DS_AP_IMPLEMENTATION
#define DS_SS_IMPLEMENTATION
#include "ds.h"

#define SYMBOL_PLUS '+'
#define SYMBOL_MINUS '-'

typedef enum {
    STACK_TOKEN_PLUS,
    STACK_TOKEN_MINUS,
    STACK_TOKEN_NUMBER,
    STACK_TOKEN_EOF,
    STACK_TOKEN_ILLEGAL,
} stack_token_kind;

static const char* stack_token_kind_to_string(stack_token_kind kind) {
    switch (kind) {
    case STACK_TOKEN_PLUS: return "+";
    case STACK_TOKEN_MINUS: return "-";
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

    return buffer;
}

int stack_token_to_asm(FILE *output, stack_token token) {
    char *buffer = NULL;
    int result = 0;

    switch (token.kind) {
    case STACK_TOKEN_PLUS: {
        fprintf(output, "   mov     rsi, qword [stack_pos]\n");
        fprintf(output, "   mov     rdi, qword [rsi - stack_sz]\n");
        fprintf(output, "   sub     qword [stack_pos], stack_sz\n");
        fprintf(output, "   mov     rsi, qword [stack_pos]\n");
        fprintf(output, "   add     rdi, qword [rsi - stack_sz]\n");
        fprintf(output, "   sub     qword [stack_pos], stack_sz\n");
        fprintf(output, "   mov     rsi, qword [stack_pos]\n");
        fprintf(output, "   mov     qword [rsi], rdi\n");
        fprintf(output, "   add     qword [stack_pos], stack_sz\n");
        return_defer(0);
    }
    case STACK_TOKEN_MINUS: {
        fprintf(output, "   mov     rsi, qword [stack_pos]\n");
        fprintf(output, "   mov     rdi, qword [rsi - stack_sz]\n");
        fprintf(output, "   sub     qword [stack_pos], stack_sz\n");
        fprintf(output, "   mov     rsi, qword [stack_pos]\n");
        fprintf(output, "   sub     rdi, qword [rsi - stack_sz]\n");
        fprintf(output, "   sub     qword [stack_pos], stack_sz\n");
        fprintf(output, "   mov     rsi, qword [stack_pos]\n");
        fprintf(output, "   mov     qword [rsi], rdi\n");
        fprintf(output, "   add     qword [stack_pos], stack_sz\n");
        return_defer(0);
    }
    case STACK_TOKEN_NUMBER: {
        if (ds_string_slice_to_owned(&token.value, &buffer) != 0) {
            return_defer(1);
        }

        fprintf(output, "   mov     rdi, %s\n", buffer);
        fprintf(output, "   mov     rsi, qword [stack_pos]\n");
        fprintf(output, "   mov     qword [rsi], rdi\n");
        fprintf(output, "   add     qword [stack_pos], stack_sz\n");

        return_defer(0);
    }
    case STACK_TOKEN_EOF: return_defer(0);
    case STACK_TOKEN_ILLEGAL:
        DS_LOG_ERROR("%s", stack_token_to_string(token));
        return_defer(1);
    }

defer:
    if (buffer != NULL) DS_FREE(NULL, buffer);
    return result;
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

static int stack_lexer_tokenize_number(stack_lexer *lexer, stack_token *token) {
    int result = 0;
    unsigned int position = lexer->pos;
    char *value = NULL;

    if (!(isdigit(lexer->ch) || lexer->ch == SYMBOL_MINUS)) {
        DS_LOG_ERROR("Failed to parse number: expected digit or '%c' but got '%c'", SYMBOL_MINUS, lexer->ch);
        return_defer(1);
    }

    ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 0 };

    if (lexer->ch == SYMBOL_MINUS) {
        slice.len += 1;
        stack_lexer_read(lexer);
    }

    while (isdigit(lexer->ch)) {
        slice.len += 1;
        stack_lexer_read(lexer);
    }

    *token = (stack_token){.kind = STACK_TOKEN_NUMBER, .value = slice, .pos = position };

defer:
    return result;
}

int stack_lexer_next(stack_lexer *lexer, stack_token *token) {
    int result = 0;
    stack_lexer_skip_whitespace(lexer);

    unsigned int position = lexer->pos;
    if (lexer->ch == EOF) {
        stack_lexer_read(lexer);
        *token = (stack_token){.kind = STACK_TOKEN_EOF, .value = {0}, .pos = position };
        return_defer(0);
    } else if (lexer->ch == SYMBOL_PLUS) {
        stack_lexer_read(lexer);
        *token = (stack_token){.kind = STACK_TOKEN_PLUS, .value = {0}, .pos = position };
        return_defer(0);
    } else if (lexer->ch == SYMBOL_MINUS && !isdigit(stack_lexer_peek(lexer))) {
        stack_lexer_read(lexer);
        *token = (stack_token){.kind = STACK_TOKEN_MINUS, .value = {0}, .pos = position };
        return_defer(0);
    } else if (isdigit(lexer->ch) || lexer->ch == SYMBOL_MINUS) {
        return_defer(stack_lexer_tokenize_number(lexer, token));
    } else {
        char *value = NULL;
        ds_string_slice slice = { .str = (char *)lexer->buffer + lexer->pos, .len = 1 };

        stack_lexer_read(lexer);

        *token = (stack_token){.kind = STACK_TOKEN_ILLEGAL, .value = slice, .pos = position };

        return_defer(0);
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
    FILE *output = stdout;

    if (argparse(argc, argv, &args) != 0) {
        DS_LOG_ERROR("Failed to parse arguments");
        return_defer(1);
    }

    buffer_len = ds_io_read(args.filename, &buffer, "r");
    if (buffer_len < 0) {
        DS_LOG_ERROR("Failed to read from file: %s", (args.filename == NULL) ? "stdin" : args.filename);
        return_defer(1);
    }

    stack_lexer lexer = {0};
    stack_token token = {0};
    stack_lexer_init(&lexer, buffer, buffer_len);

    fprintf(output, "format ELF64\n");
    fprintf(output, "\n");
    fprintf(output, "section '.data' writeable\n");
    fprintf(output, "\n");
    fprintf(output, "; memory layout\n");
    fprintf(output, "stack_pos dq 0\n");
    fprintf(output, "stack_end dq 0\n");
    fprintf(output, "\n");
    fprintf(output, "; Define some constants\n");
    fprintf(output, "stack_sz = 8\n");
    fprintf(output, "\n");
    fprintf(output, "; Define entry point\n");
    fprintf(output, "section '.text' executable\n");
    fprintf(output, "public _start\n");
    fprintf(output, "_start:\n");
    fprintf(output, "   ; Initialize the stack\n");
    fprintf(output, "   mov     rax, 12                    ; brk\n");
    fprintf(output, "   mov     rdi, 0                     ; increment = 0\n");
    fprintf(output, "   syscall\n");
    fprintf(output, "   mov     [stack_pos], rax           ; save the current position of the stack\n");
    fprintf(output, "   mov     [stack_end], rax           ; save the end of the stack\n");
    fprintf(output, "\n");
    fprintf(output, "   mov     rax, 12                    ; brk\n");
    fprintf(output, "   mov     rdi, 0x10000               ; 64K bytes (larger obj. will fail)\n");
    fprintf(output, "   add     rdi, [stack_end]           ; new end of the stack\n");
    fprintf(output, "   syscall\n");
    fprintf(output, "\n");
    fprintf(output, "   mov     [stack_end], rax           ; save the new end of the stack\n");
    fprintf(output, "\n");
    fprintf(output, "   ; Call the main method\n");
    fprintf(output, "\n");

    do {
        stack_lexer_next(&lexer, &token);
        stack_token_to_asm(output, token);
    } while (token.kind != STACK_TOKEN_EOF);

    fprintf(output, "   ; Exit the program\n");
    fprintf(output, "   mov     rax, 60\n");
    fprintf(output, "   mov     rdi, qword [stack_pos]\n");
    fprintf(output, "   mov     rdi, qword [rdi - stack_sz]\n");
    fprintf(output, "   sub     qword [stack_pos], stack_sz\n");
    fprintf(output, "   syscall\n");

defer:
    return result;
}
