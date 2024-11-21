@import stdlib

-- STACK TOKENIZER

const STACK_TOKEN_EOF       0
const STACK_TOKEN_ILLEGAL   1
const STACK_TOKEN_NUMBER    2
const STACK_TOKEN_BOOLEAN   3
const STACK_TOKEN_STRING    4
const STACK_TOKEN_NAME      5
const STACK_TOKEN_FUNC	    6
const STACK_TOKEN_IN	    7
const STACK_TOKEN_END	    8
const STACK_TOKEN_LPAREN	9
const STACK_TOKEN_RPAREN	10
const STACK_TOKEN_COMMA	    11
const STACK_TOKEN_IF	    12
const STACK_TOKEN_ELSE	    13
const STACK_TOKEN_FI	    14
const STACK_TOKEN_DATA	    15
const STACK_TOKEN_IMPORT	16
const STACK_TOKEN_CONST	    17

func stack_token_kind.map (int) (string) in
    dup STACK_TOKEN_EOF = if pop "<EOF>"
    else dup STACK_TOKEN_ILLEGAL = if pop "ILLEGAL"
    else dup STACK_TOKEN_NUMBER = if pop "NUMBER"
    else dup STACK_TOKEN_BOOLEAN = if pop "BOOLEAN"
    else dup STACK_TOKEN_STRING = if pop "STRING"
    else dup STACK_TOKEN_NAME = if pop "NAME"
    else dup STACK_TOKEN_FUNC = if pop "FUNC"
    else dup STACK_TOKEN_IN = if pop "IN"
    else dup STACK_TOKEN_END = if pop "END"
    else dup STACK_TOKEN_LPAREN = if pop "("
    else dup STACK_TOKEN_RPAREN = if pop ")"
    else dup STACK_TOKEN_COMMA = if pop ","
    else dup STACK_TOKEN_IF = if pop "IF"
    else dup STACK_TOKEN_ELSE = if pop "ELSE"
    else dup STACK_TOKEN_FI = if pop "FI"
    else dup STACK_TOKEN_DATA = if pop "DATA"
    else dup STACK_TOKEN_IMPORT = if pop "IMPORT"
    else dup STACK_TOKEN_CONST = if pop "CONST"
    else panic pop "" fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi fi -- (string)
end

const STACK_ILLEGAL_NO_ERROR    0
const STACK_ILLEGAL_INVALID     1
const STACK_ILLEGAL_STRING_NULL 2
const STACK_ILLEGAL_STRING_\N   3
const STACK_ILLEGAL_STRING_EOF  4

data stack_token (int kind, string value, int pos, int err)

func stack_token.init.ok (int, string, int) (stack_token) in -- kind, value, pos
    STACK_ILLEGAL_NO_ERROR stack_token.init
end

data stack_lexer (string buffer, int pos, int read_pos, int ch)

func stack_lexer.peek (stack_lexer) (int) in
    dup stack_lexer.buffer dup2 -- lexer, buffer, lexer, buffer

    string.len swp stack_lexer.read_pos <= if -- lexer, buffer
        pop2 BYTE_EOF
    else
        swp stack_lexer.read_pos string.!!
    fi -- byte
end

func stack_lexer.read (stack_lexer) (int) in
    dup stack_lexer.peek dup2 stack_lexer.ch.set swp -- chr, stack_lexer
    dup stack_lexer.read_pos dup2 stack_lexer.pos.set -- chr, stack_lexer, rpos
    1 + dup2 stack_lexer.read_pos.set pop2 -- chr
end

func stack_lexer.skip.whitespace (stack_lexer) () in
    dup stack_lexer.ch byte.isspace if dup stack_lexer.read pop stack_lexer.skip.whitespace else pop fi
end

func stack_lexer.skip.until_newline (stack_lexer) () in
    dup stack_lexer.ch -- stack_lexer, ch
    dup BYTE_\N = swp -- stack_lexer, bool, ch
    dup BYTE_EOF = swp -- stack_lexer, bool, bool, ch
    pop or not if -- stack_lexer
        dup stack_lexer.read pop stack_lexer.skip.until_newline -- ()
    else pop fi
end

func stack_lexer.init.with_buffer (string) (stack_lexer) in
    0 0 0 stack_lexer.init dup stack_lexer.read pop -- stack_lexer
end

func stack_lexer.next.number' (int, stack_lexer, int, string) (string) in -- pos, stack_lexer, ch, result
    swp dup byte.isdigit if -- pos, stack_lexer, string, ch
        byte.chr string.concat -- pos, stack_lexer, string
        swp dup stack_lexer.read rot stack_lexer.next.number' -- string
    else
        pop rot' pop2
    fi -- string
end

func stack_lexer.next.number (int, stack_lexer, int) (string) in -- pos, stack_lexer, ch
    dup BYTE_- = if -- pos, stack_lexer, ch
        pop dup stack_lexer.read "-"
    else
        ""
    fi -- pos, stack_lexer, ch, string

    stack_lexer.next.number' -- string
end

func stack_lexer.next.string (int, stack_lexer, int, string) (stack_token) in -- pos, stack_lexer, ch, result
    swp dup BYTE_QUOTE = not if -- pos, stack_lexer, result, ch
        dup BYTE_EOF = if -- pos, stack_lexer, result, ch
            pop3 STACK_TOKEN_ILLEGAL "" rot STACK_ILLEGAL_STRING_EOF stack_token.init
        else dup BYTE_NULL = if
            pop3 STACK_TOKEN_ILLEGAL "" rot STACK_ILLEGAL_STRING_NULL stack_token.init
        else dup BYTE_\N = if
            pop3 STACK_TOKEN_ILLEGAL "" rot STACK_ILLEGAL_STRING_\N stack_token.init
        else
            dup BYTE_\ = if -- pos, stack_lexer, result, ch
                byte.chr string.concat -- pos, stack_lexer, result
                swp dup stack_lexer.read rot swp
            fi -- pos, stack_lexer, result, ch

            byte.chr string.concat -- pos, stack_lexer, result
            swp dup stack_lexer.read rot stack_lexer.next.string -- stack_token
        fi fi fi -- stack_token
    else
        pop "\"" string.concat -- pos, stack_lexer, result
        swp stack_lexer.read pop swp -- result, pos
        STACK_TOKEN_STRING rot' stack_token.init.ok -- stack_token
    fi -- stack_token
end

func stack_lexer.next.name' (int, stack_lexer, int, string) (string) in -- pos stack_lexer, ch, result
    swp dup byte.isname if -- int, stack_lexer, result, ch
        byte.chr string.concat -- int, stack_lexer, result
        swp dup stack_lexer.read -- int, result, stack_lexer, ch
        rot stack_lexer.next.name' -- string
    else
        swp rot4' pop3
    fi
end

func stack_lexer.next.name (int, stack_lexer, int) (stack_token) in -- pos, stack_lexer, ch
    dup3 "" stack_lexer.next.name' -- int, stack_lexer, int, string

    dup "import" string.= if -- int, stack_lexer, int, string
        pop3 STACK_TOKEN_IMPORT "" rot stack_token.init.ok
    else dup "func" string.= if
        pop3 STACK_TOKEN_FUNC "" rot stack_token.init.ok
    else dup "in" string.= if
        pop3 STACK_TOKEN_IN "" rot stack_token.init.ok
    else dup "end" string.= if
        pop3 STACK_TOKEN_END "" rot stack_token.init.ok
    else dup "data" string.= if
        pop3 STACK_TOKEN_DATA "" rot stack_token.init.ok
    else dup "true" string.= if
        pop3 STACK_TOKEN_BOOLEAN "true" rot stack_token.init.ok
    else dup "false" string.= if
        pop3 STACK_TOKEN_BOOLEAN "false" rot stack_token.init.ok
    else dup "if" string.= if
        pop3 STACK_TOKEN_IF "" rot stack_token.init.ok
    else dup "else" string.= if
        pop3 STACK_TOKEN_ELSE "" rot stack_token.init.ok
    else dup "fi" string.= if
        pop3 STACK_TOKEN_FI "" rot stack_token.init.ok
    else dup "const" string.= if
        pop3 STACK_TOKEN_CONST "" rot stack_token.init.ok
    else dup "@import" string.= if
        pop3 STACK_TOKEN_IMPORT "" rot stack_token.init.ok
    else
        rot4' pop2 STACK_TOKEN_NAME rot' stack_token.init.ok
    fi fi fi fi fi fi fi fi fi fi fi fi -- stack_token
end

func stack_lexer.next (stack_lexer) (stack_token) in
    dup stack_lexer.skip.whitespace -- stack_lexer
    dup stack_lexer.pos swp -- pos, stack_lexer
    dup stack_lexer.ch -- pos, stack_lexer, ch
    dup BYTE_EOF = if -- pos, stack_lexer, ch
        pop stack_lexer.read pop STACK_TOKEN_EOF "" rot stack_token.init.ok
    else dup BYTE_LPAREN = if
        pop stack_lexer.read pop STACK_TOKEN_LPAREN "" rot stack_token.init.ok
    else dup BYTE_RPAREN = if
        pop stack_lexer.read pop STACK_TOKEN_RPAREN "" rot stack_token.init.ok
    else dup BYTE_COMMA = if
        pop stack_lexer.read pop STACK_TOKEN_COMMA "" rot stack_token.init.ok
    else dup BYTE_- = rot dup stack_lexer.peek BYTE_- = rot and swp rot' if
        swp dup stack_lexer.skip.until_newline -- pos, ch, stack_lexer
        rot' pop2 stack_lexer.next
    else dup byte.isdigit rot' dup BYTE_- = rot dup stack_lexer.peek byte.isdigit rot and swp rot4' rot or if
        dup3 stack_lexer.next.number -- pos, stack_lexer, ch, string
        rot' pop2 swp STACK_TOKEN_NUMBER rot' stack_token.init.ok
    else dup BYTE_QUOTE = if
        pop dup stack_lexer.read "\"" stack_lexer.next.string
    else dup byte.isname if
        stack_lexer.next.name -- stack_token
    else
        int.show swp stack_lexer.read pop STACK_TOKEN_ILLEGAL swp rot STACK_ILLEGAL_INVALID stack_token.init
    fi fi fi fi fi fi fi fi -- stack_token
end

func stack_lexer.dump (stack_lexer) () in
    dup stack_lexer.next -- stack_lexer, token

    dup stack_token.kind -- stack_lexer, token, kind
    stack_token_kind.map -- stack_lexer, token, s

    swp dup stack_token.value dup string.len 0 > if -- stack_lexer, s, token, value
        rot "(" string.concat swp string.concat ")" string.concat
    else
        pop swp
    fi -- stack_lexer, token, s

    "\n" string.concat string.stdout stack_token.kind -- stack_lexer, kind
    STACK_TOKEN_EOF = not if stack_lexer.dump else pop fi -- ()
end

-- STACK PARSER

const STACK_AST_EXPR_NUMBER 0
const STACK_AST_EXPR_BOOLEAN 1
const STACK_AST_EXPR_STRING 2
const STACK_AST_EXPR_NAME 3
const STACK_AST_EXPR_COND 4

data stack_parser (stack_lexer lexer, stack_token tok, stack_token next_tok)

data stack_ast_node (string value)
data stack_ast_cond (stack_ast_node cond, array if_, array else_)
data stack_ast_expr (int kind, ptr expr)
data stack_ast_func (stack_ast_node name, array exprs)
data stack_ast (array funcs)

-- STACK ASSEMBLER

const STACK_LITERAL_NUMBER 0
const STACK_LITERAL_BOOLEAN 1
const STACK_LITERAL_STRING 2

data stack_literal (int kind, string value)

data stack_assembler (int fd, array func_map, array literal_map, int if_counter)

func stack_literal.= (stack_literal, stack_literal) (bool) in
    dup2 stack_literal.kind swp stack_literal.kind = rot' -- bool, l1, l2
    stack_literal.value swp stack_literal.value string.= and -- bool
end

func stack_assembler.init.with_fd (int) (stack_assembler) in
    string.sizeof array.init.with_sz
    stack_literal.sizeof array.init.with_sz
    0

    stack_assembler.init -- stack_assembler
end

func emit (stack_assembler, string) () in -- asm, s
    swp stack_assembler.fd swp -- fd, s
    "\n" string.concat -- fd, s
    stdlib.fwrite
    not if panic fi
end

func stack_assembler.func.name' (int, string, array) (int) in -- i, name, array<string>
    dup array.count -- i, name, array<string>, c
    rot4 dup rot -- name, array<string>, i, i, c
    >= if -- name, array<string>, i
        rot' swp string.& array.append not if panic fi -- i
    else
        dup2 array.get not if panic fi -- name, array<string>, i, ptr
        string.* -- name, array<string>, i, string
        rot4 dup rot string.= if -- array<string>, i, name
            rot pop2  -- i
        else
            swp 1 + swp -- array<string>, i+1, name
            rot stack_assembler.func.name' -- int
        fi -- int
    fi -- int
end

func stack_assembler.func.name (stack_assembler, string) (string) in -- asm, name
    swp stack_assembler.func_map -- string, array<string>
    0 rot' stack_assembler.func.name' -- int
    int.show "func." swp string.concat -- string
end

func stack_assembler.literal.name' (int, stack_literal, array) (int) in -- i, name, array<stack_literal>
    dup array.count -- i, literal, array<stack_literal>, c
    rot4 dup rot -- literal, array<stack_literal>, i, i, c
    >= if -- literal, array<stack_literal>, i
        rot' swp stack_literal.& array.append not if panic fi -- i
    else
        dup2 array.get not if panic fi -- stack_literal, array<string>, i, ptr
        stack_literal.* -- literal, array<stack_literal>, i, item
        rot4 dup rot stack_literal.= if -- array<stack_literal>, i, literal
            rot pop2  -- i
        else
            swp 1 + swp -- array<stack_literal>, i+1, literal
            rot stack_assembler.literal.name' -- int
        fi -- int
    fi -- int
end

func stack_assembler.literal.name (stack_assembler, stack_literal) (string) in -- asm, value
    swp stack_assembler.literal_map -- stack_literal, array<stack_literal>
    0 rot' stack_assembler.literal.name' -- int
    int.show "literal." swp string.concat -- string
end

func stack_assembler.emit.expr.number (stack_assembler, stack_ast_node) () in -- asm, number
    swp dup rot -- asm, asm, node
    stack_ast_node.value dup2 -- asm, asm, value, asm, value
    STACK_LITERAL_NUMBER swp stack_literal.init -- asm, asm, value, asm, stack_literal
    stack_assembler.literal.name -- asm, asm, value, s
    "    mov     rdi, " swp string.concat " ; " string.concat swp string.concat emit

    dup "    call    stack_push_addr" emit

    pop
end

func stack_assembler.emit.expr.boolean (stack_assembler, stack_ast_node) () in -- asm, boolean
    swp dup rot -- asm, asm, node
    stack_ast_node.value dup2 -- asm, asm, value, asm, value
    STACK_LITERAL_BOOLEAN swp stack_literal.init -- asm, asm, value, asm, stack_literal
    stack_assembler.literal.name -- asm, asm, value, s
    "    mov     rdi, " swp string.concat " ; " string.concat swp string.concat emit

    dup "    call    stack_push_addr" emit

    pop
end

func stack_assembler.emit.expr.string (stack_assembler, stack_ast_node) () in -- asm, string
    swp dup rot -- asm, asm, node
    stack_ast_node.value dup2 -- asm, asm, value, asm, value
    STACK_LITERAL_STRING swp stack_literal.init -- asm, asm, value, asm, stack_literal
    stack_assembler.literal.name -- asm, asm, value, s
    "    mov     rdi, " swp string.concat " ; " string.concat swp string.concat emit

    dup "    call    stack_push_addr" emit

    pop
end

func stack_assembler.emit.expr.name (stack_assembler, stack_ast_node) () in -- asm, name
    dup stack_ast_node.value rot dup rot stack_assembler.func.name "    call    " swp string.concat

    swp dup rot emit

    pop2
end

func stack_assembler.emit.expr.cond (stack_assembler, stack_ast_cond) () in -- asm, cond
    swp dup stack_assembler.if_counter swp -- cond, i, asm

    dup "    call    stack_pop" emit
    dup "    test    rax, rax" emit

    dup2 swp -- cond, i, asm, asm, i
    int.show "    jnz     .if" swp string.concat emit -- cond, i, asm
    dup2 swp -- cond, i, asm, asm, i
    int.show ".else" swp string.concat ":" string.concat emit -- cond, i, asm

    rot dup2 stack_ast_cond.else_ stack_assembler.emit.exprs rot' -- cond, i, asm

    dup2 swp -- cond i, asm, asm, i
    int.show "    jmp    .fi" swp string.concat emit -- cond, i, asm
    dup2 swp -- cond i, asm, asm, i
    int.show ".if" swp string.concat ":" string.concat emit -- cond, i, asm

    rot dup2 stack_ast_cond.if_ stack_assembler.emit.exprs rot' -- cond, i, asm

    dup2 swp -- cond, i, asm, asm, i
    int.show ".fi" swp string.concat ":" string.concat emit -- cond, i, asm


    swp 1 + stack_assembler.if_counter.set pop
end

func stack_assembler.emit.expr (stack_assembler, stack_ast_expr) () in -- asm, expr
    dup stack_ast_expr.kind  -- asm, expr, kind
    dup STACK_AST_EXPR_NUMBER = if -- asm, expr, kind
        pop stack_ast_expr.expr stack_ast_node.* stack_assembler.emit.expr.number
    else dup STACK_AST_EXPR_BOOLEAN = if -- asm, expr, kind
        pop stack_ast_expr.expr stack_ast_node.* stack_assembler.emit.expr.boolean
    else dup STACK_AST_EXPR_STRING = if -- asm, expr, kind
        pop stack_ast_expr.expr stack_ast_node.* stack_assembler.emit.expr.string
    else dup STACK_AST_EXPR_NAME = if -- asm, expr, kind
        pop stack_ast_expr.expr stack_ast_node.* stack_assembler.emit.expr.name
    else dup STACK_AST_EXPR_COND = if -- asm, expr, kind
        pop stack_ast_expr.expr stack_ast_cond.* stack_assembler.emit.expr.cond
    else
        panic pop3
    fi fi fi fi fi -- ()
end

func stack_assembler.emit.exprs' (int, stack_assembler, array) () in -- asm, array<expr>
    dup array.count -- i, asm, array<expr>, c
    rot4 dup rot -- asm, array<expr>, i, i, c
    >= if -- asm, array<expr>, i
        pop3
    else
        dup2 array.get not if panic fi -- asm, array<expr>, i, ptr
        stack_ast_expr.* -- asm, array<expr>, i, expr
        rot4 dup rot stack_assembler.emit.expr rot' -- asm, array<expr>, i
        1 + rot' -- i+1, asm, array<expr>
        stack_assembler.emit.exprs' -- ()
    fi -- ()
end

func stack_assembler.emit.exprs (stack_assembler, array) () in 0 rot' stack_assembler.emit.exprs' end

func stack_assembler.emit.func (stack_assembler, stack_ast_func) () in -- asm, func
    dup stack_ast_func.name -- asm, func, node
    stack_ast_node.value -- asm, func, string
    rot dup rot4' swp stack_assembler.func.name ":" string.concat rot' -- label, asm, func

    swp
    dup rot4                    emit
    dup "    push    rbp"       emit
    dup "    mov     rbp, rsp"  emit
    dup ""                      emit

    dup2 swp stack_ast_func.exprs stack_assembler.emit.exprs

    dup ""                      emit
    dup "    pop     rbp"       emit
    dup "    ret"               emit
    dup ""                      emit

    pop2
end

func stack_assembler.emit.ast.funcs' (int, stack_assembler, array) () in -- i, asm, a
    dup array.count -- i, asm, array<func>, c
    rot4 dup rot -- asm, array<func>, i, i, c
    >= if -- asm, array<func>, i
        pop3
    else
        dup2 array.get not if panic fi -- asm, array<func>, i, ptr
        stack_ast_func.* -- asm, array<func>, i, func
        rot4 dup rot stack_assembler.emit.func rot' -- asm, array<func>, i
        1 + rot' -- i+1, asm, array<func>
        stack_assembler.emit.ast.funcs' -- ()
    fi -- ()
end

func stack_assembler.emit.ast.funcs (stack_assembler, array) () in 0 rot' stack_assembler.emit.ast.funcs' end

func stack_assembler.emit.ast (stack_assembler, stack_ast) () in -- asm, prog
    swp
    dup "section '.text' executable" emit
    dup ""                           emit

    dup2 swp stack_ast.funcs stack_assembler.emit.ast.funcs

    pop2
end

func stack_assembler.emit.string.interpret' (int, string, string) (string) in -- i, string, result
    rot' dup2 string.len < if -- result, i, string
        dup2 swp string.!! -- result, i, string, chr
        BYTE_\ = if -- result, i, string
            dup2 swp -- result, i, string, string, i
            1 + string.!! -- result, i, string, chr+1
            dup BYTE_n = if -- result, i, string, chr+1
                pop "\n"
            else dup BYTE_t = if
                pop "\t"
            else dup BYTE_b = if
                pop "\b"
            else dup BYTE_f = if
                pop "\f"
            else
                pop dup2 swp 1 + 1 string.substr -- result, i, string, s+1
            fi fi fi fi -- result, i, string, s
            rot 2 + rot' -- result, i+2, string, s
        else
            dup2 swp 1 string.substr rot 1 + rot' -- result, i+1, string, s
        fi -- result, i+x, string, s
        rot4 swp string.concat -- i+x, string, result
        stack_assembler.emit.string.interpret'
    else
        pop2
    fi -- string
end

func stack_assembler.emit.string.interpret (string) (string) in -- string
    dup string.len 2 - 1 swp string.substr -- s\"
    0 swp "" stack_assembler.emit.string.interpret' -- string
end

func stack_assembler.emit.literal.string.helper' (int, string, string) (string) in -- i, value, result
    rot' dup2 string.len < if -- result, i, value
        swp dup 0 > if -- result, value, i
            rot "," string.concat rot' swp
        else
            swp
        fi -- result, i, value

        dup2 swp string.!! int.show -- result, i, value, b
        rot 1 + rot' -- result, i+1, value, b
        rot4 swp string.concat stack_assembler.emit.literal.string.helper' -- string
    else
        pop2
    fi -- string
end

func stack_assembler.emit.literal.string.helper (string, string) (string) in -- str, value
    -- str value len
    dup string.len  -- str, value, len
    dup 0 = if -- str, value, len
        pop2 "0" string.concat
    else
        swp 0 swp "" stack_assembler.emit.literal.string.helper' -- str, len, bs
        ",0" rot -- str, bs, 0" len
        int.sizeof -- str, bs, 0" len, 8
        swp -- str, bs, 0", 8, len
        int.sizeof mod -- str, bs, 0", 8, len%8
        - -- str, bs, 0", 8-len%8
        string.repeat -- str bs 0s
        string.concat string.concat -- string
    fi -- string
end

func stack_assembler.emit.literal (stack_assembler, stack_literal, int) () in -- asm, literal, i
    rot' -- i, asm, literal

    dup stack_literal.value swp stack_literal.kind -- i, asm, value, kind
    dup STACK_LITERAL_NUMBER = if -- i, asm, value, kind
        pop "literal." rot4 int.show string.concat " dq " string.concat swp string.concat emit -- ()
    else dup STACK_LITERAL_BOOLEAN = if -- i, asm, value, kind
        pop "literal." rot4 int.show string.concat " dq " string.concat swp "true" string.= if "1" else "0" fi string.concat emit -- ()
    else dup STACK_LITERAL_STRING = if -- i, asm, value, kind
        pop stack_assembler.emit.string.interpret -- i, asm, value\"
        "literal." rot4 int.show dup rot4' string.concat " dq " string.concat swp dup string.len int.show rot swp string.concat rot4 dup rot emit  -- i, value, asm
        dup3 "          dq string." rot4 string.concat emit pop -- i, value, asm
        dup3 "string." rot4 string.concat " db " string.concat swp pop swp  stack_assembler.emit.literal.string.helper dup2 emit pop -- i, value, asm

        pop3
    else
        panic pop3 pop
    fi fi fi -- ()
end

func stack_assembler.emit.literals' (int, stack_assembler, array) () in -- i, asm, array<stack_literal>
    dup array.count -- i, asm, array<stack_literal>, c
    rot4 dup rot -- asm, array<stack_literal>, i, i, c
    >= if -- asm, array<stack_literal>, i
        pop3
    else
        dup2 array.get not if panic fi -- asm, array<stack_literal>, i, ptr
        stack_literal.* -- asm, array<stack_literal>, i, stack_literal
        rot4 dup rot rot4 dup rot4' stack_assembler.emit.literal -- array<stack_literal>, asm, i
        rot swp 1 + rot' -- i+1, asm, array<stack_literal>
        stack_assembler.emit.literals' -- ()
    fi -- ()
end

func stack_assembler.emit.literals (stack_assembler) () in -- asm
    dup "section '.data'" emit
    dup "" emit

    dup stack_assembler.literal_map -- stack_assembler, array<stack_literal>
    0 rot' stack_assembler.emit.literals' -- ()
end

func stack_assembler.emit (stack_assembler, stack_ast) () in -- asm, ast
    swp
    dup "format ELF64" emit
    dup ""             emit

    dup stack_assembler.emit.allocator
    dup stack_assembler.emit.entry
    dup stack_assembler.emit.keywords
    swp dup2 stack_assembler.emit.ast
    swp dup stack_assembler.emit.literals

    pop2
end

data stack_args (string filename, bool lexer)

func stack_args.init.empty () (stack_args) in
    "" false stack_args.init
end

func stack_args.usage () () in
    "usage: slc [option] [input]\n" string.stdout
    "stack lang compiler\n" string.stdout
    "\n" string.stdout
    "options:\n" string.stdout
    "  -h, --help\n" string.stdout
    "      print this help message\n" string.stdout
    "\n" string.stdout
    "  -l, --lexer\n" string.stdout
    "      flag to stop on the lexer phase\n" string.stdout
    "\n" string.stdout
    "  i, input\n" string.stdout
    "      the input file\n" string.stdout
    "\n" string.stdout
end

func stack_args.parse' (array, int, stack_args) (stack_args) in -- array, i, args
    swp rot dup2 array.count >= if -- args, i, array
        pop2
    else
        -- args array i string
        swp dup2 array.get not if panic fi ptr.* dup ptr.strlen swp string.init -- args, array, i, string

        dup "--help" string.= swp dup "-h" string.= rot or if -- args, array, i, string
            pop stack_args.usage 0 sys.exit
        else dup "--lexer" string.= swp dup "-l" string.= rot or if
            rot4 dup true stack_args.lexer.set rot4' pop
        else
            -- array, i, string args L
            rot4 dup stack_args.filename string.len 0 > if -- array, i, string, args
                rot4' stack_args.usage
                "Got unexpected CLI argument: `" swp string.concat "`\n" string.concat string.stderr 1 sys.exit
            else
                dup rot stack_args.filename.set rot'
            fi -- args, array, i
        fi fi -- args, array, i

        1 + rot -- array, i+1, args
        stack_args.parse' -- args
    fi -- args
end

func stack_args.parse (int, ptr) (stack_args) in -- argc, argv
    swp dup ptr.sizeof rot4 array.init 1 stack_args.init.empty stack_args.parse'
end

func main (int, ptr) (int) in -- argc, argv
    stack_args.parse -- args

    dup stack_args.filename dup string.len 0 = if -- args, filename
        pop STDOUT
    else
        "r" stdlib.fopen not if panic fi
    fi -- args, fd

    stdlib.fread.<eof> not if panic fi -- args, string
    stack_lexer.init.with_buffer -- args, stack_lexer
    dup stack_lexer.dump -- args, stack_lexer

    pop2

    0
end

func stack_assembler.emit.entry (stack_assembler) () in
    dup "section '.data' writeable" emit
    dup "" emit
    dup "; Define some constants" emit
    dup "loc_0 = 8" emit
    dup "loc_1 = 16" emit
    dup "loc_2 = 24" emit
    dup "loc_3 = 32" emit
    dup "loc_4 = 40" emit
    dup "loc_5 = 48" emit
    dup "loc_6 = 56" emit
    dup "loc_7 = 64" emit
    dup "" emit
    dup "arg_0 = 16" emit
    dup "arg_1 = 24" emit
    dup "arg_2 = 32" emit
    dup "arg_3 = 40" emit
    dup "arg_4 = 48" emit
    dup "arg_5 = 56" emit
    dup "arg_6 = 64" emit
    dup "arg_7 = 72" emit
    dup "" emit
    dup "; Define entry point" emit
    dup "section '.text' executable" emit
    dup "public _start" emit
    dup "_start:" emit
    dup "    ; Initialize the memory" emit
    dup "    call allocator_init" emit
    dup "" emit
    dup "    ; Call the main method" emit

    dup dup STACK_FUNC_MAIN stack_assembler.func.name "    call   " swp string.concat emit
    dup "" emit
    dup "    ; Exit the program" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    mov     rax, 60" emit
    dup "    syscall" emit
    dup "" emit

    pop
end

func stack_assembler.emit.allocator (stack_assembler) () in
    dup "section '.data' writeable" emit
    dup "" emit

    dup "; memory layout" emit
    dup "stack_pos dq 0" emit
    dup "stack_end dq 0" emit
    dup "heap_pos dq 0" emit
    dup "heap_end dq 0" emit
    dup "" emit

    dup "section '.text' executable" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; allocator_init" emit
    dup ";" emit
    dup ";   INPUT: nothing" emit
    dup ";   OUTPUT: nothing" emit
    dup "allocator_init:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "" emit
    dup "    ; allocate the stack 64K" emit
    dup "    mov     rax, 12                    ; brk" emit
    dup "    mov     rdi, 0                     ; increment = 0" emit
    dup "    syscall" emit
    dup "    mov     [stack_pos], rax           ; save the current position of the stack" emit
    dup "    mov     [stack_end], rax           ; save the end of the stack" emit
    dup "" emit
    dup "    mov     rax, 12                    ; brk" emit
    dup "    mov     rdi, 0x10000               ; 64K bytes (larger obj. will fail)" emit
    dup "    add     rdi, [stack_end]           ; new end of the stack" emit
    dup "    syscall" emit
    dup "" emit
    dup "    ; initialize the heap" emit
    dup "    mov     rax, 12                    ; brk" emit
    dup "    mov     rdi, 0                     ; increment = 0" emit
    dup "    syscall" emit
    dup "    mov     [heap_pos], rax            ; save the current position of the heap" emit
    dup "    mov     [heap_end], rax            ; save the end of the heap" emit
    dup "" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; stack push addr" emit
    dup ";" emit
    dup ";   INPUT: rdi contains the int64 (pointer) that we add to the stack" emit
    dup ";   OUTPUT: nothing" emit
    dup ";" emit
    dup "stack_push_addr:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "" emit
    dup "    mov     rsi, qword [stack_pos]" emit
    dup "    mov     qword [rsi], rdi" emit
    dup "    add     qword [stack_pos], 8" emit
    dup "" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; stack push" emit
    dup ";" emit
    dup ";   INPUT: rdi contains the int64 (pointer) that we add to the stack" emit
    dup ";   OUTPUT: nothing" emit
    dup ";" emit
    dup "stack_push:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "" emit
    dup "    push    rdi" emit
    dup "    mov     rdi, 8" emit
    dup "    call    allocate" emit
    dup "    pop     rdi" emit
    dup "    mov     [rax], rdi" emit
    dup "" emit
    dup "    mov     rsi, qword [stack_pos]" emit
    dup "    mov     qword [rsi], rax" emit
    dup "    add     qword [stack_pos], 8" emit
    dup "" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; stack peek addr" emit
    dup ";" emit
    dup ";   INPUT: nothing" emit
    dup ";   OUTPUT: rax contains the int64 (pointer) that we pop from the stack" emit
    dup ";" emit
    dup "stack_peek_addr:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "" emit
    dup "    mov     rax, qword [stack_pos]" emit
    dup "    mov     rax, qword [rax - 8]" emit
    dup "" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; stack peek" emit
    dup ";" emit
    dup ";   INPUT: nothing" emit
    dup ";   OUTPUT: rax contains the int64 (pointer) that we pop from the stack" emit
    dup ";" emit
    dup "stack_peek:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "" emit
    dup "    mov     rax, qword [stack_pos]" emit
    dup "    mov     rax, qword [rax - 8]" emit
    dup "    mov     rax, qword [rax]" emit
    dup "" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; stack pop addr" emit
    dup ";" emit
    dup ";   INPUT: nothing" emit
    dup ";   OUTPUT: rax contains the int64 (pointer) that we pop from the stack" emit
    dup ";" emit
    dup "stack_pop_addr:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "" emit
    dup "    mov     rax, qword [stack_pos]" emit
    dup "    mov     rax, qword [rax - 8]" emit
    dup "    sub     qword [stack_pos], 8" emit
    dup "" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; stack pop" emit
    dup ";" emit
    dup ";   INPUT: nothing" emit
    dup ";   OUTPUT: rax contains the int64 (pointer) that we pop from the stack" emit
    dup ";" emit
    dup "stack_pop:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "" emit
    dup "    mov     rax, qword [stack_pos]" emit
    dup "    mov     rax, qword [rax - 8]" emit
    dup "    mov     rax, qword [rax]" emit
    dup "    sub     qword [stack_pos], 8" emit
    dup "" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    dup ";" emit
    dup ";" emit
    dup "; allocate" emit
    dup ";" emit
    dup ";   INPUT: rdi contains the size in bytes" emit
    dup ";   OUTPUT: rax points to the newly allocated memory" emit
    dup ";" emit
    dup "allocate:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    ; t0 <- heap_pos" emit
    dup "    mov     rax, qword [heap_pos]" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- t0 + rdi" emit
    dup "    mov     rax, qword [rbp - loc_0]" emit
    dup "    add     rax, rdi" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup ".alloc_do:" emit
    dup "    ; cmp t1 <= heap_end" emit
    dup "    mov     rax, qword [rbp - loc_1]" emit
    dup "    cmp     rax, qword [heap_end]" emit
    dup "    jle     .alloc_ok" emit
    dup "" emit
    dup "    mov     rax, 12                    ; brk" emit
    dup "    mov     rdi, 0x10000               ; 64K bytes (larger obj. will fail)" emit
    dup "    add     rdi, [heap_end]            ; new end of the heap" emit
    dup "    syscall" emit
    dup "" emit
    dup "    mov     [heap_end], rax            ; save the new end of the heap" emit
    dup "    jmp     .alloc_do" emit
    dup "" emit
    dup ".alloc_ok:" emit
    dup "" emit
    dup "    ; heap_pos <- t1" emit
    dup "    mov     rax, qword [rbp - loc_1]" emit
    dup "    mov     qword [heap_pos], rax" emit
    dup "" emit
    dup "    ; return t0" emit
    dup "    mov     rax, qword [rbp - loc_0]" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    pop
end

const STACK_FUNC_MAIN "main"

const STACK_FUNC_DUP "dup"
const STACK_FUNC_SWP "swp"
const STACK_FUNC_ROT "rot"
const STACK_FUNC_ROT4 "rot4"
const STACK_FUNC_POP "pop"

const STACK_FUNC_PLUS "+"
const STACK_FUNC_MINUS "-"
const STACK_FUNC_STAR "*"
const STACK_FUNC_DIV "/"
const STACK_FUNC_MOD "%"
const STACK_FUNC_OR "|"
const STACK_FUNC_AND "&"
const STACK_FUNC_XOR "^"
const STACK_FUNC_SHR ">>"
const STACK_FUNC_SHL "<<"

const STACK_FUNC_GT ">"
const STACK_FUNC_LT "<"
const STACK_FUNC_EQ "="

const STACK_FUNC_PTR_ALLOC "ptr.alloc"
const STACK_FUNC_PTR_OFFSET "ptr.+"
const STACK_FUNC_PTR_COPY "ptr.@"

const STACK_FUNC_SYSCALL1 "syscall1"
const STACK_FUNC_SYSCALL3 "syscall3"

func stack_assembler.emit.keywords (stack_assembler) () in
    dup "section '.text' executable" emit
    dup "" emit

    -- DUP
    dup ";" emit
    dup ";" emit
    dup "; dup" emit
    dup ";" emit
    dup ";   INPUT: (a)" emit
    dup ";   OUTPUT: (a, a)" emit
    dup dup STACK_FUNC_DUP dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_peek_addr" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- SWAP
    dup ";" emit
    dup ";" emit
    dup "; swp" emit
    dup ";" emit
    dup ";   INPUT: (a, b)" emit
    dup ";   OUTPUT: (b, a)" emit
    dup dup STACK_FUNC_SWP dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    ; t0 <- A" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- B" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; push A" emit
    dup "    mov     rdi, [rbp - loc_0]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    ; push B" emit
    dup "    mov     rdi, [rbp - loc_1]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- ROT
    dup ";" emit
    dup ";" emit
    dup "; rot" emit
    dup ";" emit
    dup ";   INPUT: (a, b, c)" emit
    dup ";   OUTPUT: (b, c, a)" emit
    dup dup STACK_FUNC_ROT dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 32                    ; allocate 4 local variables" emit
    dup "" emit
    dup "    ; (C B A) -> (B A C)" emit
    dup "" emit
    dup "    ; t0 <- A" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- B" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; t2 <- C" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_2], rax" emit
    dup "" emit
    dup "    ; push B" emit
    dup "    mov     rdi, [rbp - loc_1]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    ; push A" emit
    dup "    mov     rdi, [rbp - loc_0]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    ; push C" emit
    dup "    mov     rdi, [rbp - loc_2]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    add     rsp, 32                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- ROT4
    dup ";" emit
    dup ";" emit
    dup "; rot4" emit
    dup ";" emit
    dup ";   INPUT: (a, b, c, d)" emit
    dup ";   OUTPUT: (b, c, d, a)" emit
    dup dup STACK_FUNC_ROT4 dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 32                    ; allocate 4 local variables" emit
    dup "" emit
    dup "    ; (D C B A) -> (C B A D)" emit
    dup "" emit
    dup "    ; t0 <- A" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- B" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; t2 <- C" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_2], rax" emit
    dup "" emit
    dup "    ; t3 <- D" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     qword [rbp - loc_3], rax" emit
    dup "" emit
    dup "    ; push C" emit
    dup "    mov     rdi, [rbp - loc_2]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    ; push B" emit
    dup "    mov     rdi, [rbp - loc_1]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    ; push A" emit
    dup "    mov     rdi, [rbp - loc_0]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    ; push D" emit
    dup "    mov     rdi, [rbp - loc_3]" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    add     rsp, 32                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- POP
    dup ";" emit
    dup ";" emit
    dup "; pop" emit
    dup ";" emit
    dup ";   INPUT: (a)" emit
    dup ";   OUTPUT: ()" emit
    dup dup STACK_FUNC_POP dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop_addr" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    -- PLUS
    dup ";" emit
    dup ";" emit
    dup "; plus" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_PLUS dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rdi" emit
    dup "" emit
    dup "    add     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- SUB
    dup ";" emit
    dup ";" emit
    dup "; sub" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_MINUS dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    pop     rax" emit
    dup "" emit
    dup "    sub     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- MUL
    dup ";" emit
    dup ";" emit
    dup "; mul" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_STAR dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rdi" emit
    dup "" emit
    dup "    mul     rdi" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- DIV
    dup ";" emit
    dup ";" emit
    dup "; div" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_DIV dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rdi" emit
    dup "" emit
    dup "    cqo" emit
    dup "    idiv    rdi" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- MOD
    dup ";" emit
    dup ";" emit
    dup "; mod" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_MOD dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rdi" emit
    dup "" emit
    dup "    cqo" emit
    dup "    idiv    rdi" emit
    dup "    mov     rdi, rdx" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- AND
    dup ";" emit
    dup ";" emit
    dup "; and" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_AND dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rdi" emit
    dup "" emit
    dup "    and     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- OR
    dup ";" emit
    dup ";" emit
    dup "; or" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_OR dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rdi" emit
    dup "" emit
    dup "    or      rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- XOR
    dup ";" emit
    dup ";" emit
    dup "; xor" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_XOR dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rdi" emit
    dup "" emit
    dup "    xor     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- SHR
    dup ";" emit
    dup ";" emit
    dup "; shr" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_SHR dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rcx" emit
    dup "" emit
    dup "    shr     ax, cl" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- SHL
    dup ";" emit
    dup ";" emit
    dup "; shl" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_SHL dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    pop     rcx" emit
    dup "" emit
    dup "    shl     ax, cl" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- GT
    dup ";" emit
    dup ";" emit
    dup "; greater than" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (bool)" emit
    dup dup STACK_FUNC_GT dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    pop     rax" emit
    dup "" emit
    dup "    cmp     rdi, rax" emit
    dup "    setg    al" emit
    dup "    and     al, 1" emit
    dup "    movzx   rax, al" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- LT
    dup ";" emit
    dup ";" emit
    dup "; less than" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (bool)" emit
    dup dup STACK_FUNC_LT dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    pop     rax" emit
    dup "" emit
    dup "    cmp     rdi, rax" emit
    dup "    setl    al" emit
    dup "    and     al, 1" emit
    dup "    movzx   rax, al" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- EQ
    dup ";" emit
    dup ";" emit
    dup "; less than" emit
    dup ";" emit
    dup ";   INPUT: (int, int)" emit
    dup ";   OUTPUT: (bool)" emit
    dup dup STACK_FUNC_EQ dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    push    rax" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    pop     rax" emit
    dup "" emit
    dup "    cmp     rdi, rax" emit
    dup "    sete    al" emit
    dup "    and     al, 1" emit
    dup "    movzx   rax, al" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    -- PTR ALLOC
    dup ";" emit
    dup ";" emit
    dup "; memory allocate" emit
    dup ";" emit
    dup ";   INPUT: (int)" emit
    dup ";   OUTPUT: (ptr)" emit
    dup dup STACK_FUNC_PTR_ALLOC dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 16                    ; allocate 2 local variables" emit
    dup "" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    call    allocate" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 16                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- PTR OFFSET
    dup ";" emit
    dup ";" emit
    dup "; memory offset" emit
    dup ";" emit
    dup ";   INPUT: (ptr, int)" emit
    dup ";   OUTPUT: (ptr)" emit
    dup dup STACK_FUNC_PTR_OFFSET dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 24                    ; allocate 3 local variables" emit
    dup "" emit
    dup "    ; t1 <- int" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; t2 <- ptr" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_2], rax" emit
    dup "" emit
    dup "    ; ptr[0] <- byte a" emit
    dup "    mov     rax, qword [rbp - loc_2]" emit
    dup "    mov     rdi, qword [rbp - loc_1]" emit
    dup "    add     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 24                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- PTR COPY
    dup ";" emit
    dup ";" emit
    dup "; memory copy" emit
    dup ";" emit
    dup ";   INPUT: (dst, src, len)" emit
    dup ";   OUTPUT: ()" emit
    dup dup STACK_FUNC_PTR_COPY dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 24                    ; allocate 3 local variables" emit
    dup "" emit
    dup "    ; t0 <- len" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- src" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; t2 <- dst" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_2], rax" emit
    dup "" emit
    dup "    mov     rdi, qword [rbp - loc_2]" emit
    dup "    mov     rsi, qword [rbp - loc_1]" emit
    dup "    mov     rdx, qword [rbp - loc_0]" emit
    dup "" emit
    dup ".next_byte:" emit
    dup "    cmp     rdx, 0                     ; check if done" emit
    dup "    jle     .done" emit
    dup "" emit
    dup "    mov     al, byte [rsi]             ; get byte from self" emit
    dup "    mov     byte [rdi], al             ; copy byte to new object" emit
    dup "" emit
    dup "    inc     rdi                        ; increment destination" emit
    dup "    inc     rsi                        ; increment source" emit
    dup "    dec     rdx                        ; decrement count" emit
    dup "" emit
    dup "    jmp .next_byte" emit
    dup ".done:" emit
    dup "" emit
    dup "    mov     rdi, qword [rbp - loc_2]" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 24                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    -- SYSCALL1
    dup ";" emit
    dup ";" emit
    dup "; syscall1" emit
    dup ";" emit
    dup ";   INPUT: (a, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_SYSCALL1 dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 32                    ; allocate 4 local variables" emit
    dup "" emit
    dup "    ; t0 <- int" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- a" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; syscall1(t0) t1" emit
    dup "    mov     rax, qword [rbp - loc_0]" emit
    dup "    mov     rdi, qword [rbp - loc_1]" emit
    dup "    syscall" emit
    dup "" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 32                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- SYSCALL3
    dup ";" emit
    dup ";" emit
    dup "; syscall3" emit
    dup ";" emit
    dup ";   INPUT: (a, b, c, int)" emit
    dup ";   OUTPUT: (int)" emit
    dup dup STACK_FUNC_SYSCALL3 dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 32                    ; allocate 4 local variables" emit
    dup "" emit
    dup "    ; t0 <- int" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- c" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; t2 <- b" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_2], rax" emit
    dup "" emit
    dup "    ; t3 <- a" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_3], rax" emit
    dup "" emit
    dup "    ; syscall3(t0) t3 t2 t1" emit
    dup "    mov     rax, qword [rbp - loc_0]" emit
    dup "    mov     rdi, qword [rbp - loc_3]" emit
    dup "    mov     rsi, qword [rbp - loc_2]" emit
    dup "    mov     rdx, qword [rbp - loc_1]" emit
    dup "    syscall" emit
    dup "" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 32                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    pop
end
