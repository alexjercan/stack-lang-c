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

func stack_token.init.empty () (stack_token) in
    0 "" 0 0 stack_token.init
end

func stack_token.init.ok (int, string, int) (stack_token) in -- kind, value, pos
    STACK_ILLEGAL_NO_ERROR stack_token.init
end

data stack_lexer (string buffer, int pos, int read_pos, int ch, string filename)

func stack_lexer.init.with_buffer (string, string) (stack_lexer) in -- buffer, filename
    0 0 0 rot4 stack_lexer.init dup stack_lexer.read pop -- stack_lexer
end

func stack_lexer.pos.to_lc' (stack_lexer, int) (int, int) in -- lexer, pos
    dup 0 >= if -- lexer, pos
        swp dup stack_lexer.buffer -- pos, lexer, string
        -- lexer, pos
        rot dup rot swp string.!! BYTE_\N = if -- lexer, pos
            1 - stack_lexer.pos.to_lc' -- line, col
            pop 1 + 1 -- line+1, 1
        else
            1 - stack_lexer.pos.to_lc' -- line, col
            1 + -- line, col+1
        fi
    else
        pop2 1 1
    fi -- int, int
end

func stack_lexer.pos.to_lc (stack_lexer, int) (int, int) in -- lexer, pos
    1 - stack_lexer.pos.to_lc'
end

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
    dup stack_lexer.pos 0 + swp -- pos, stack_lexer
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

data stack_parser (stack_lexer lexer, stack_token tok, stack_token next_tok)

func stack_parser.init.with_lexer (stack_lexer) (stack_parser) in
    stack_token.init.empty stack_token.init.empty stack_parser.init
    dup stack_parser.read pop
end

func stack_parser.peek (stack_parser) (stack_token) in stack_parser.next_tok end

func stack_parser.read (stack_parser) (stack_token) in
    dup dup stack_parser.peek stack_parser.tok.set
    dup dup stack_parser.lexer stack_lexer.next stack_parser.next_tok.set
    stack_parser.tok
end

func stack_parser.parse.exprs.until (stack_parser, int, array) (array) in
    rot' dup2 swp stack_parser.peek stack_token.kind = if -- arr p kind
        pop stack_parser.read pop -- array
    else
        swp dup stack_parser.parse.expr unwrap -- arr kind p expr
        rot4 dup rot stack_ast_expr.& array.append unwrap -- kind parser arr
        rot' swp rot stack_parser.parse.exprs.until -- array
    fi -- array
end

func stack_parser.parse.exprs.while (stack_parser, array) (array) in -- parser, array<expr>
    swp dup stack_parser.parse.expr if -- array, parser, expr
        rot dup rot stack_ast_expr.& array.append unwrap -- parser, array
        stack_parser.parse.exprs.while -- array
    else
        pop2
    fi -- array
end

func stack_parser.parse.exprs.until2 (stack_parser, int, int, array) (array) in
    rot4' dup3 rot dup stack_parser.peek stack_token.kind rot4 =
    swp dup stack_parser.peek stack_token.kind rot4 = rot or if -- arr, parser, k1, k2, parser
        pop3 stack_parser.read pop -- array
    else
        pop rot4 rot4 dup stack_parser.parse.expr unwrap -- k1, k2, array, parser, expr
        rot dup rot stack_ast_expr.& array.append unwrap -- k1, k2, parser, array
        swp rot4' stack_parser.parse.exprs.until2 -- array
    fi -- array
end

func stack_parser.parse.names.until' (stack_parser, array) (array) in
    swp dup stack_parser.read -- arr, p, tok

    dup stack_token.kind STACK_TOKEN_NAME = unwrap -- arr, p, tok

    dup2 stack_token.value rot stack_token.pos stack_ast_node.init -- arr, p, node
    rot dup rot stack_ast_node.& array.append unwrap -- parser arr

    swp dup stack_parser.read dup stack_token.kind STACK_TOKEN_RPAREN = if -- arr, parser, tok
        pop2
    else dup stack_token.kind STACK_TOKEN_COMMA = if
        pop swp stack_parser.parse.names.until'
    else
        panic pop2
    fi fi -- array
end

func stack_parser.parse.names.until (stack_parser, array) (array) in
    swp dup stack_parser.peek stack_token.kind STACK_TOKEN_RPAREN = if -- arr p
        stack_parser.read pop -- array
    else
        swp stack_parser.parse.names.until'
    fi -- array
end

func stack_parser.parse.cond (stack_parser) (stack_ast_cond) in
    dup stack_parser.read -- parser, tok
    dup2 stack_token.value rot stack_token.pos stack_ast_node.init -- parser, node

    swp dup STACK_TOKEN_ELSE STACK_TOKEN_FI stack_ast_expr.sizeof array.init.with_sz -- node, parser, parser, k1, k2, array
    stack_parser.parse.exprs.until2 -- node, parser, if_

    swp -- node, if_, parser

    dup stack_parser.tok stack_token.kind STACK_TOKEN_ELSE = if -- node, if_, parser
        dup STACK_TOKEN_FI stack_ast_expr.sizeof array.init.with_sz -- node, if_, parser, parser, k, array
        stack_parser.parse.exprs.until -- node, if_, parser, else_
        swp
    else
        stack_ast_expr.sizeof array.init.with_sz -- node, if_, parser, array
        swp
    fi -- node, if_, else_, parser

    pop stack_ast_cond.init -- cond
end

func stack_parser.illegal.showf (stack_parser) () in
    dup stack_parser.lexer stack_lexer.filename string.stdout ":" string.stdout

    dup dup stack_parser.lexer swp stack_parser.peek stack_token.pos -- parser, lexer, pos
    stack_lexer.pos.to_lc -- parser, line, col
    swp int.show string.stdout ":" string.stdout int.show string.stdout
    ", " string.stdout "Lexical Error: " string.stdout

    dup stack_parser.peek dup stack_token.err -- parser, tok, err
    dup STACK_ILLEGAL_INVALID = if -- parser, tok, err
        "Invalid character: " string.stdout pop2
    else dup STACK_ILLEGAL_STRING_NULL = if
        "String contains null character" string.stdout pop2
    else dup STACK_ILLEGAL_STRING_\N = if
        "String contains new line character" string.stdout pop2
    else dup STACK_ILLEGAL_STRING_EOF = if
        "Unterminated string" string.stdout pop2
    else
        panic pop2
    fi fi fi fi -- parser

    "\n" string.stdout

    pop
end

func stack_parser.parse.expr (stack_parser) (stack_ast_expr, bool) in
    dup stack_parser.peek stack_token.kind STACK_TOKEN_NAME = if -- parser
        dup stack_parser.read -- parser, tok
        dup stack_token.value swp stack_token.pos stack_ast_node.init -- node
        STACK_AST_EXPR_NAME swp stack_ast_node.& stack_ast_expr.init -- expr
        true
    else dup stack_parser.peek stack_token.kind STACK_TOKEN_NUMBER = if
        dup stack_parser.read -- parser, tok
        dup stack_token.value swp stack_token.pos stack_ast_node.init -- node
        STACK_AST_EXPR_NUMBER swp stack_ast_node.& stack_ast_expr.init -- expr
        true
    else dup stack_parser.peek stack_token.kind STACK_TOKEN_BOOLEAN = if
        dup stack_parser.read -- parser, tok
        dup stack_token.value swp stack_token.pos stack_ast_node.init -- node
        STACK_AST_EXPR_BOOLEAN swp stack_ast_node.& stack_ast_expr.init -- expr
        true
    else dup stack_parser.peek stack_token.kind STACK_TOKEN_STRING = if
        dup stack_parser.read -- parser, tok
        dup stack_token.value swp stack_token.pos stack_ast_node.init -- node
        STACK_AST_EXPR_STRING swp stack_ast_node.& stack_ast_expr.init -- expr
        true
    else dup stack_parser.peek stack_token.kind STACK_TOKEN_IF = if
        stack_parser.parse.cond -- stack_ast_cond
        STACK_AST_EXPR_COND swp stack_ast_cond.& stack_ast_expr.init -- expr
        true
    else dup stack_parser.peek stack_token.kind STACK_TOKEN_ILLEGAL = if
        dup stack_parser.illegal.showf
        pop 0 0 int.& stack_ast_expr.init false
    else
        pop 0 0 int.& stack_ast_expr.init false
    fi fi fi fi fi fi -- expr, ok
end

func stack_parser.parse.func (stack_parser) (stack_ast_func) in -- parser
    dup stack_parser.read stack_token.kind STACK_TOKEN_FUNC = unwrap -- parser
    dup stack_parser.read dup stack_token.kind STACK_TOKEN_NAME = unwrap -- parser tok
    dup2 stack_token.value rot stack_token.pos stack_ast_node.init swp -- name, parser

    dup stack_parser.read stack_token.kind STACK_TOKEN_LPAREN = unwrap -- name, parser
    dup stack_ast_node.sizeof array.init.with_sz stack_parser.parse.names.until swp -- name, array<stack_ast_node>, parser

    dup stack_parser.read stack_token.kind STACK_TOKEN_LPAREN = unwrap -- name, args, parser
    dup stack_ast_node.sizeof array.init.with_sz stack_parser.parse.names.until swp -- name, args, rets, parser

    dup stack_parser.read stack_token.kind STACK_TOKEN_IN = unwrap -- name, parser
    dup STACK_TOKEN_END stack_ast_expr.sizeof array.init.with_sz stack_parser.parse.exprs.until swp pop -- name, args, rets, exprs
    stack_ast_func.init -- func
end

func stack_parser.parse.data_field.until' (stack_parser, array) (array) in -- p, arr
    swp -- arr, p

    dup stack_parser.read -- arr, p, tok
    dup stack_token.kind STACK_TOKEN_NAME = unwrap -- arr, p, tok
    dup2 stack_token.value rot stack_token.pos stack_ast_node.init rot' -- type, arr, p

    dup stack_parser.read -- type, arr, p, tok
    dup stack_token.kind STACK_TOKEN_NAME = unwrap -- type, arr, p, tok
    dup2 stack_token.value rot stack_token.pos stack_ast_node.init rot' -- type, name, arr, p

    rot4' rot4' stack_ast_data_field.init -- arr, p, field
    rot dup rot stack_ast_data_field.& array.append unwrap -- parser arr

    swp dup stack_parser.read dup stack_token.kind STACK_TOKEN_RPAREN = if -- arr, parser, tok
        pop2
    else dup stack_token.kind STACK_TOKEN_COMMA = if
        pop swp stack_parser.parse.data_field.until'
    else
        panic pop2
    fi fi -- array
end

func stack_parser.parse.data_field.until (stack_parser, array) (array) in
    swp dup stack_parser.peek stack_token.kind STACK_TOKEN_RPAREN = if -- arr p
        stack_parser.read pop -- array
    else
        swp stack_parser.parse.data_field.until'
    fi -- array
end

func stack_parser.parse.data (stack_parser) (stack_ast_data) in -- parser
    dup stack_parser.read stack_token.kind STACK_TOKEN_DATA = unwrap -- parser
    dup stack_parser.read dup stack_token.kind STACK_TOKEN_NAME = unwrap -- parser tok
    dup2 stack_token.value rot stack_token.pos stack_ast_node.init swp -- name, parser

    dup stack_parser.read stack_token.kind STACK_TOKEN_LPAREN = unwrap -- name, parser
    dup stack_ast_data_field.sizeof array.init.with_sz stack_parser.parse.data_field.until swp -- name, array<stack_ast_data_field>, parser

    pop -- name, array
    stack_ast_data.init -- data
end

func stack_parser.parse.const (stack_parser) (stack_ast_const) in -- parser
    dup stack_parser.read stack_token.kind STACK_TOKEN_CONST = unwrap -- parser
    dup stack_parser.read dup stack_token.kind STACK_TOKEN_NAME = unwrap -- parser tok
    dup2 stack_token.value rot stack_token.pos stack_ast_node.init swp -- name, parser

    dup stack_ast_expr.sizeof array.init.with_sz stack_parser.parse.exprs.while swp -- name, array<stack_ast_data_field>, parser

    pop -- name, array
    stack_ast_const.init -- const
end

func stack_parser.parse.import (stack_parser) (stack_ast_import) in -- parser
    dup stack_parser.read stack_token.kind STACK_TOKEN_IMPORT = unwrap -- parser
    dup stack_parser.read dup stack_token.kind STACK_TOKEN_NAME = unwrap -- parser tok
    dup2 stack_token.value rot stack_token.pos stack_ast_node.init swp -- name, parser

    pop -- name
    stack_ast_import.init -- import
end

func stack_parser.parse' (stack_parser, stack_ast) (stack_ast) in -- parser, ast
    swp dup stack_parser.peek dup stack_token.kind STACK_TOKEN_EOF = if -- ast, parser, tok
        pop2
    else
        dup stack_token.kind STACK_TOKEN_DATA = if -- ast, parser, tok
            pop dup stack_parser.parse.data -- ast, parser, data
            rot dup rot stack_ast.features.append.data -- parser, ast
        else dup stack_token.kind STACK_TOKEN_FUNC = if
            pop dup stack_parser.parse.func -- ast, parser, func
            rot dup rot stack_ast.features.append.func -- parser, ast
        else dup stack_token.kind STACK_TOKEN_IMPORT = if
            pop dup stack_parser.parse.import -- ast, parser, import
            rot dup rot stack_ast.features.append.import -- parser, ast
        else dup stack_token.kind STACK_TOKEN_CONST = if
            pop dup stack_parser.parse.const -- ast, parser, const
            rot dup rot stack_ast.features.append.const -- parser, ast
        else
            panic pop swp
        fi fi fi fi -- parser, ast

        stack_parser.parse' -- ast
    fi -- ast
end

func stack_parser.parse (stack_parser) (stack_ast, bool) in -- ast
    stack_ast.init.empty -- stack_parser, ast
    stack_parser.parse' true -- ast, ok
end

data stack_ast_node (stack_parser parser, string value, int pos)

data stack_ast_cond (stack_ast_node cond, array if_, array else_)

const STACK_AST_EXPR_NUMBER 0
const STACK_AST_EXPR_BOOLEAN 1
const STACK_AST_EXPR_STRING 2
const STACK_AST_EXPR_NAME 3
const STACK_AST_EXPR_COND 4

data stack_ast_expr (int kind, ptr expr)

data stack_ast_data_field (stack_ast_node type, stack_ast_node name)

data stack_ast_data (stack_ast_node name, array fields)
data stack_ast_func (stack_ast_node name, array args, array rets, array exprs)
data stack_ast_const (stack_ast_node name, array exprs)
data stack_ast_import (stack_ast_node name)

const STACK_AST_FEATURE_DATA 0
const STACK_AST_FEATURE_FUNC 1
const STACK_AST_FEATURE_CONST 2
const STACK_AST_FEATURE_IMPORT 3

data stack_ast_feature (int kind, ptr feature)

data stack_ast (array features)

func stack_ast.init.empty () (stack_ast) in
    stack_ast_func.sizeof array.init.with_sz stack_ast.init
end

func stack_ast.append (stack_ast, stack_ast) () in -- ast, ast'
    stack_ast.features swp stack_ast.features swp array.extend unwrap
end

func stack_ast.features.append (stack_ast, stack_ast_feature) () in
    swp stack_ast.features swp stack_ast_feature.& array.append unwrap
end

func stack_ast.features.append.func (stack_ast, stack_ast_func) () in
    stack_ast_func.& STACK_AST_FEATURE_FUNC swp stack_ast_feature.init stack_ast.features.append
end

func stack_ast.features.append.data (stack_ast, stack_ast_data) () in
    stack_ast_data.& STACK_AST_FEATURE_DATA swp stack_ast_feature.init stack_ast.features.append
end

func stack_ast.features.append.const (stack_ast, stack_ast_const) () in
    stack_ast_const.& STACK_AST_FEATURE_CONST swp stack_ast_feature.init stack_ast.features.append
end

func stack_ast.features.append.import (stack_ast, stack_ast_import) () in
    stack_ast_import.& STACK_AST_FEATURE_IMPORT swp stack_ast_feature.init stack_ast.features.append
end

const stack_ast.dump.indent 4

func stack_ast.dump.cond (int, stack_ast_cond) () in -- indent, cond
    swp dup " " swp string.repeat string.stdout "IF\n" string.stdout -- cond, indent

    dup stack_ast.dump.indent + -- cond, indent, indent+
    rot dup stack_ast_cond.if_ rot swp -- indent, cond, indent+, if_
    stack_ast.dump.exprs swp -- cond, indent

    dup " " swp string.repeat string.stdout "ELSE\n" string.stdout

    dup stack_ast.dump.indent + -- cond, indent, indent+
    rot dup stack_ast_cond.else_ rot swp -- indent, cond, indent+, else_
    stack_ast.dump.exprs swp -- cond, indent

    dup " " swp string.repeat string.stdout "FI\n" string.stdout

    pop2
end

func stack_ast.dump.exprs' (int, int, array) () in -- indent, i, exprs
    dup2 array.count < if -- indent, i, exprs
        dup2 swp array.get unwrap stack_ast_expr.* -- indent, i, exprs, expr
        rot4 swp -- i, exprs, indent, expr

        dup stack_ast_expr.kind dup STACK_AST_EXPR_NUMBER = if -- i, exprs, indent, expr, kind
            pop dup2 swp " " swp string.repeat string.stdout
            stack_ast_expr.expr stack_ast_node.* stack_ast_node.value string.stdout "\n" string.stdout pop
        else dup STACK_AST_EXPR_BOOLEAN = if
            pop dup2 swp " " swp string.repeat string.stdout
            stack_ast_expr.expr stack_ast_node.* stack_ast_node.value string.stdout "\n" string.stdout pop
        else dup STACK_AST_EXPR_STRING = if
            pop dup2 swp " " swp string.repeat string.stdout
            stack_ast_expr.expr stack_ast_node.* stack_ast_node.value string.stdout "\n" string.stdout pop
        else dup STACK_AST_EXPR_NAME = if
            pop dup2 swp " " swp string.repeat string.stdout
            stack_ast_expr.expr stack_ast_node.* stack_ast_node.value string.stdout "\n" string.stdout pop
        else dup STACK_AST_EXPR_COND = if
            pop stack_ast_expr.expr stack_ast_cond.*  -- i, exprs, indent, cond
            swp dup rot -- i, exprs, indent, indent, cond
            stack_ast.dump.cond -- i, exprs, indent
        else
            panic pop2
        fi fi fi fi fi -- i, exprs, indent

        rot 1 + rot stack_ast.dump.exprs' -- ()
    else
        pop3
    fi -- ()
end

func stack_ast.dump.exprs (int, array) () in 0 swp stack_ast.dump.exprs' end

func stack_ast.dump.args' (int, int, array) () in -- indent, i, names
    dup2 array.count < if -- indent, i, name
        dup2 swp array.get unwrap stack_ast_node.* -- indent, i, names, node

        rot4 dup " " swp string.repeat string.stdout -- i, names, node, indent
        rot4 dup int.show "arg" swp string.concat string.stdout ": " string.stdout -- names, node, indent, i
        rot stack_ast_node.value string.stdout "\n" string.stdout -- names, indent, i

        1 + rot stack_ast.dump.args' -- ()
    else
        pop3
    fi -- ()
end

func stack_ast.dump.args (int, array) () in 0 swp stack_ast.dump.args' end

func stack_ast.dump.rets' (int, int, array) () in -- indent, i, names
    dup2 array.count < if -- indent, i, name
        dup2 swp array.get unwrap stack_ast_node.* -- indent, i, names, node

        rot4 dup " " swp string.repeat string.stdout -- i, names, node, indent
        rot4 dup int.show "ret" swp string.concat string.stdout ": " string.stdout -- names, node, indent, i
        rot stack_ast_node.value string.stdout "\n" string.stdout -- names, indent, i

        1 + rot stack_ast.dump.rets' -- ()
    else
        pop3
    fi -- ()
end

func stack_ast.dump.rets (int, array) () in 0 swp stack_ast.dump.rets' end

func stack_ast.dump.fields' (int, int, array) () in -- indent, i, array<stack_ast_data_field>
    dup2 array.count < if -- indent, i, fields
        dup2 swp array.get unwrap stack_ast_data_field.* -- indent, i, fields, field
        rot4 dup " " swp string.repeat string.stdout -- i, fields, field, indent
        swp -- i, fields, indent, field

        dup stack_ast_data_field.name stack_ast_node.value string.stdout ": " string.stdout -- i, fields, indent, field
        dup stack_ast_data_field.type stack_ast_node.value string.stdout "\n" string.stdout -- i, fields, indent, field

        pop rot 1 + rot stack_ast.dump.fields' -- ()
    else
        pop3
    fi -- ()
end

func stack_ast.dump.fields (int, array) () in 0 swp stack_ast.dump.fields' end

func stack_ast.dump' (int, array) () in
    dup2 array.count < if -- i, array
        dup2 swp array.get unwrap stack_ast_feature.* -- i, array, feat
        dup stack_ast_feature.kind STACK_AST_FEATURE_DATA = if -- i, array, feat
            "data " string.stdout
            stack_ast_feature.feature stack_ast_data.* dup stack_ast_data.name stack_ast_node.value string.stdout -- i, array, data
            "\n" string.stdout

            dup stack_ast_data.fields -- i, array, data, fields
            stack_ast.dump.indent swp stack_ast.dump.fields -- i, array, data
            "\n" string.stdout

            pop
        else dup stack_ast_feature.kind STACK_AST_FEATURE_FUNC = if -- i, array, feat
            "func " string.stdout
            stack_ast_feature.feature stack_ast_func.* dup stack_ast_func.name stack_ast_node.value string.stdout -- i, array, func
            "\n" string.stdout

            dup stack_ast_func.args -- i, array, func, args
            stack_ast.dump.indent swp stack_ast.dump.args -- i, array, func

            dup stack_ast_func.rets -- i, array, func, rets
            stack_ast.dump.indent swp stack_ast.dump.rets -- i, array, func

            dup stack_ast_func.exprs -- i, array, func, exprs
            " " stack_ast.dump.indent string.repeat string.stdout "body:\n" string.stdout
            stack_ast.dump.indent stack_ast.dump.indent + swp stack_ast.dump.exprs -- i, array, func
            "\n" string.stdout

            pop
        else dup stack_ast_feature.kind STACK_AST_FEATURE_CONST = if -- i, array, feat
            "const " string.stdout
            stack_ast_feature.feature stack_ast_const.* dup stack_ast_const.name stack_ast_node.value string.stdout -- i, array, const
            "\n" string.stdout

            dup stack_ast_const.exprs -- i, array, const, exprs
            stack_ast.dump.indent swp stack_ast.dump.exprs -- i, array, const
            "\n" string.stdout

            pop
        else dup stack_ast_feature.kind STACK_AST_FEATURE_IMPORT = if -- i, array, feat
            "@import " string.stdout
            stack_ast_feature.feature stack_ast_const.* dup stack_ast_const.name stack_ast_node.value string.stdout -- i, array, import
            "\n\n" string.stdout

            pop
        else
            dup stack_ast_feature.kind int.show string.stdout "\n" string.stdout
            todo pop
        fi fi fi fi -- i, array

        swp 1 + swp stack_ast.dump'
    else
        pop2
    fi -- ()
end

func stack_ast.dump (stack_ast) () in stack_ast.features 0 swp stack_ast.dump' end

-- STACK PREPROCESSOR

data stack_preprocessor ()

func stack_home () (string) in
    -- TODO: get env for "STACK_HOME"
    "."
end

const STACK_DATA_INT "int"
const STACK_DATA_PTR "ptr"
const STACK_DATA_BOOL "bool"

const STACK_SIZEOF "sizeof"

const STACK_SPECIAL_FILE "__file__"
const STACK_SPECIAL_LINE "__line__"
const STACK_SPECIAL_COL "__col__"

func stack_preprocessor.run.base.consts (stack_preprocessor, stack_ast) () in -- pre, ast
    "" "stack.sl" stack_lexer.init.with_buffer -- lexer
    stack_parser.init.with_lexer -- pre, ast, parser

    dup dup -- pre, ast, parser, parser, parser
    STACK_DATA_INT "." string.concat STACK_SIZEOF string.concat -- parser, parser, value
    0 stack_ast_node.init swp -- sizeof_node parser
    int.sizeof int.show -- sizeof_node, parser, string
    0 stack_ast_node.init -- sizeof_node, 8_node
    STACK_AST_EXPR_NUMBER -- sizeof_node, 8_node, kind
    swp stack_ast_node.& stack_ast_expr.init -- sizeof_node, 8_expr
    stack_ast_expr.sizeof array.init.with_sz -- sizeof_node, expr, array
    dup rot stack_ast_expr.& array.append unwrap -- sizeof_node, array
    stack_ast_const.init -- pre, ast, parser, int.sizeof
    dup3 rot swp stack_ast.features.append.const pop pop -- pre, ast, parser

    dup dup -- pre, ast, parser, parser, parser
    STACK_DATA_PTR "." string.concat STACK_SIZEOF string.concat -- parser, parser, value
    0 stack_ast_node.init swp -- sizeof_node parser
    int.sizeof int.show -- sizeof_node, parser, string
    0 stack_ast_node.init -- sizeof_node, 8_node
    STACK_AST_EXPR_NUMBER -- sizeof_node, 8_node, kind
    swp stack_ast_node.& stack_ast_expr.init -- sizeof_node, 8_expr
    stack_ast_expr.sizeof array.init.with_sz -- sizeof_node, expr, array
    dup rot stack_ast_expr.& array.append unwrap -- sizeof_node, array
    stack_ast_const.init -- pre, ast, parser, int.sizeof
    dup3 rot swp stack_ast.features.append.const pop pop -- pre, ast, parser

    dup dup -- pre, ast, parser, parser, parser
    STACK_DATA_BOOL "." string.concat STACK_SIZEOF string.concat -- parser, parser, value
    0 stack_ast_node.init swp -- sizeof_node parser
    int.sizeof int.show -- sizeof_node, parser, string
    0 stack_ast_node.init -- sizeof_node, 8_node
    STACK_AST_EXPR_NUMBER -- sizeof_node, 8_node, kind
    swp stack_ast_node.& stack_ast_expr.init -- sizeof_node, 8_expr
    stack_ast_expr.sizeof array.init.with_sz -- sizeof_node, expr, array
    dup rot stack_ast_expr.& array.append unwrap -- sizeof_node, array
    stack_ast_const.init -- pre, ast, parser, int.sizeof
    dup3 rot swp stack_ast.features.append.const pop pop -- pre, ast, parser

    pop3
end

func stack_preprocessor.run.import (int, stack_preprocessor, stack_ast) () in -- i, pre, ast
    rot swp -- pre, i, ast
    dup stack_ast.features dup array.count rot4 dup rot < if -- pre ast array<feat> i
        dup2 array.get unwrap stack_ast_feature.* dup stack_ast_feature.kind rot4 pop -- pre, ast, i, feat, kind
        dup STACK_AST_FEATURE_IMPORT = if -- pre, ast, i, feat, kind
            pop stack_ast_feature.feature stack_ast_import.* stack_ast_import.name stack_ast_node.value stack_home -- pre ast i filename home
            "/lib" string.concat "/" string.concat swp string.concat ".sl" string.concat -- pre ast i path
            dup "r" stdlib.fopen unwrap -- pre ast i path fd
            dup stdlib.fread.<eof> unwrap -- pre ast i path, fd, string
            swp stdlib.fclose unwrap -- pre ast i path string

            swp stack_lexer.init.with_buffer -- pre ast i stack_lexer
            stack_parser.init.with_lexer -- pre ast i stack_parser
            stack_parser.parse unwrap -- pre ast i, ast'

            stack_preprocessor.init -- pre ast i ast', pre'
            dup2 swp stack_preprocessor.run' pop -- pre ast i ast'

            rot dup rot stack_ast.append -- pre i ast
            swp rot'
        else
            pop pop rot'
        fi -- i, pre, ast

        rot 1 + rot' stack_preprocessor.run.import
    else
        pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.update (int, array, stack_ast_node) () in
    rot' -- node, i, array
    dup array.count -- node, i, array, L
    rot swp -- node, array i L
    dup2 < if -- node array i L
        pop
        dup2 array.get unwrap -- node array i ptr
        stack_ast_expr.* dup stack_ast_expr.kind -- node array i expr kind
        dup STACK_AST_EXPR_NAME = -- node array i expr kind bool
        swp dup STACK_AST_EXPR_NUMBER = rot or -- node array i expr kind bool
        swp dup STACK_AST_EXPR_STRING = rot or -- node array i expr kind bool
        swp dup STACK_AST_EXPR_BOOLEAN = rot or if -- node array i expr kind
            pop -- node array i expr
            stack_ast_expr.expr stack_ast_node.* -- node array i node'
            rot4 -- array i node' node

            dup2 -- ..., node' node
            stack_ast_node.pos stack_ast_node.pos.set -- ...

            dup2 -- ..., node', node
            stack_ast_node.parser stack_ast_node.parser.set -- ...

            swp pop
        else dup STACK_AST_EXPR_COND = if
            pop -- node array i expr

            rot4 swp -- array i node expr
            stack_ast_expr.expr stack_ast_cond.* -- array i node cond

            dup2  -- ..., node cond
            stack_ast_cond.if_ -- ..., node if_
            0 swp rot -- ..., 0 if_ node
            stack_preprocessor.run.expand.update -- ...

            dup2  -- ..., node cond
            stack_ast_cond.else_ -- ..., node else_
            0 swp rot -- ..., 0 if_ node
            stack_preprocessor.run.expand.update -- ...

            pop
        else
            todo pop pop rot
        fi fi -- array i node

        swp 1 + -- array node i+1
        rot' stack_preprocessor.run.expand.update
    else
        pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.exprs (int, array, stack_ast_const) () in -- i, exprs, const
    rot' -- const, i, array
    dup array.count -- const, i, array, L
    rot swp -- const array i L
    dup2 < if -- const array i L
        pop -- const array i
        dup2 array.get unwrap -- const array i ptr
        stack_ast_expr.* dup stack_ast_expr.kind -- const array i expr kind
        dup STACK_AST_EXPR_NAME = if -- const array i expr kind
            pop -- const array i expr
            rot4 swp -- array i const expr
            stack_ast_expr.expr stack_ast_node.* -- array i const node
            stack_ast_node.value -- array i const name
            swp dup -- array i name const const
            stack_ast_const.name stack_ast_node.value -- array i name const name_c
            rot string.= if -- array i const
                dup3 -- ...
                stack_ast_const.exprs -- ..., array i exprs
                rot' -- ..., exprs array i
                dup2 -- ..., exprs, array, i, array i
                array.get unwrap stack_ast_expr.* -- ..., exprs, array, i, name
                rot' dup2 -- ..., exprs, name, array, i, array i
                array.delete unwrap -- ... exprs, name, array, i
                rot4 rot4 -- ..., array, i, exprs, name

                dup2 0 rot' -- ..., array, i, exprs, name, 0, exprs, name
                stack_ast_expr.expr stack_ast_node.*
                stack_preprocessor.run.expand.update -- ..., array, i, exprs, name

                pop -- ..., array, i, exprs

                array.insert_many unwrap -- ...
            fi -- array i const

            rot'
        else dup STACK_AST_EXPR_COND = if
            pop -- const array i expr

            rot4 swp -- array i const expr
            stack_ast_expr.expr stack_ast_cond.* -- array i const cond

            dup2  -- ..., const cond
            stack_ast_cond.if_ -- ..., const if_
            0 swp rot -- ..., 0 if_ const
            stack_preprocessor.run.expand.exprs -- ...

            dup2 -- ..., const cond
            stack_ast_cond.else_ -- ..., const else_
            0 swp rot -- ..., 0 else_ const
            stack_preprocessor.run.expand.exprs -- ...

            pop rot'
        else
            pop pop
        fi fi -- const array i

        1 + rot' swp stack_preprocessor.run.expand.exprs
    else
        pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.consts' (int, stack_preprocessor, stack_ast, stack_ast_const) () in -- i, pre, ast, const
    swp rot4 swp -- pre, const, i, ast
    dup stack_ast.features dup array.count rot4 dup rot < if -- pre const ast array<feat> i
        dup2 array.get unwrap stack_ast_feature.* dup stack_ast_feature.kind rot4 pop -- pre, const, ast, i, feat, kind
        dup STACK_AST_FEATURE_CONST = if -- pre, const, ast, i, feat, kind
            pop stack_ast_feature.feature -- pre, const, ast, i, ptr
            stack_ast_const.* -- pre, const, ast, i, const'
            stack_ast_const.exprs -- pre, const, ast, i, exprs'
            rot4 dup rot' -- pre, ast, i, const, exprs', const
            0 rot' -- pre, ast, i, const, 0, exprs', const
            stack_preprocessor.run.expand.exprs -- pre, ast, i, const
            swp rot4'
        else
            pop pop rot4' swp
        fi -- i, pre, ast, const

        rot4 1 + rot4' stack_preprocessor.run.expand.consts'
    else
        pop pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.consts  (int, stack_preprocessor, stack_ast) () in -- i, pre, ast
    rot swp -- pre, i, ast
    dup stack_ast.features dup array.count rot4 dup rot < if -- pre ast array<feat> i
        dup2 array.get unwrap stack_ast_feature.* dup stack_ast_feature.kind rot4 pop -- pre, ast, i, feat, kind
        dup STACK_AST_FEATURE_CONST = if -- pre, ast, i, feat, kind
            pop stack_ast_feature.feature -- pre, ast, i, ptr
            stack_ast_const.* -- pre, ast, i, const
            swp rot4' -- i, pre, ast, const
            dup3 -- ..., pre, ast, const
            0 rot4' -- ..., j, pre, ast, const
            stack_preprocessor.run.expand.consts' pop -- i, pre, ast
        else
            pop pop rot'
        fi -- i, pre, ast

        rot 1 + rot' stack_preprocessor.run.expand.consts
    else
        pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.funcs' (int, stack_preprocessor, stack_ast, stack_ast_const) () in -- i, pre, ast, const
    swp rot4 swp -- pre, const, i, ast
    dup stack_ast.features dup array.count rot4 dup rot < if -- pre const ast array<feat> i
        dup2 array.get unwrap stack_ast_feature.* dup stack_ast_feature.kind rot4 pop -- pre, const, ast, i, feat, kind
        dup STACK_AST_FEATURE_FUNC = if -- pre, const, ast, i, feat, kind
            pop stack_ast_feature.feature -- pre, const, ast, i, ptr
            stack_ast_func.* -- pre, const, ast, i, func'
            stack_ast_func.exprs -- pre, const, i, exprs'
            rot4 dup rot' -- pre, ast, i, const, exprs', const
            0 rot' -- pre, ast, i, const, 0, exprs', const
            stack_preprocessor.run.expand.exprs -- pre, ast, i, const
            swp rot4'
        else
            pop pop rot4' swp
        fi -- i, pre, ast, const

        rot4 1 + rot4' stack_preprocessor.run.expand.funcs'
    else
        pop pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.funcs (int, stack_preprocessor, stack_ast) () in -- i, pre, ast
    rot swp -- pre, i, ast
    dup stack_ast.features dup array.count rot4 dup rot < if -- pre ast array<feat> i
        dup2 array.get unwrap stack_ast_feature.* dup stack_ast_feature.kind rot4 pop -- pre, ast, i, feat, kind
        dup STACK_AST_FEATURE_CONST = if -- pre, ast, i, feat, kind
            pop stack_ast_feature.feature -- pre, ast, i, ptr
            stack_ast_const.* -- pre, ast, i, const
            swp rot4' -- i, pre, ast, const
            dup3 -- ..., pre, ast, const
            0 rot4' -- ..., j, pre, ast, const
            stack_preprocessor.run.expand.funcs' pop -- i, pre, ast
        else
            pop pop rot'
        fi -- i, pre, ast

        rot 1 + rot' stack_preprocessor.run.expand.funcs
    else
        pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.special.exprs (int, array) () in -- i, array<expr>
    dup array.count rot dup rot < if -- array<feat>, i
        dup2 -- ..., arr, i
        array.get unwrap stack_ast_expr.* -- ..., expr
        dup stack_ast_expr.kind -- ..., expr, kind
        dup STACK_AST_EXPR_NAME = if -- ..., expr, kind
            pop stack_ast_expr.expr stack_ast_node.* -- ..., node
            dup stack_ast_node.value -- ..., node, name
            dup STACK_SPECIAL_FILE string.= if -- ..., node, name
                pop -- ..., node
                dup -- ..., node, node
                stack_ast_node.parser -- ..., node, parser
                swp stack_ast_node.pos -- ..., parser, pos
                swp dup -- ..., pos, parser, parser
                stack_parser.lexer -- ... pos, parser, lexer
                stack_lexer.filename -- ... pos, parser, filename
                "\"" swp string.concat "\"" string.concat -- pos, parser, "filename"
                rot stack_ast_node.init -- array, i, node
                STACK_AST_EXPR_STRING -- array, i, node, kind
                swp stack_ast_node.& -- array, i, kind, ptr
                stack_ast_expr.init -- array, i, expr
                rot' dup2 array.delete unwrap rot -- array, i, expr
                dup3 -- ..., expr, array, i, expr
                stack_ast_expr.& array.insert unwrap pop -- array, i
            else dup STACK_SPECIAL_LINE string.= if
                pop -- ..., node
                dup -- ..., node, node
                stack_ast_node.parser -- ..., node, parser
                dup stack_parser.lexer -- ..., node, parser, lexer
                rot stack_ast_node.pos -- ..., parser, lexer, pos
                dup rot swp  -- ..., parser, pos, lexer, pos
                stack_lexer.pos.to_lc pop -- ..., parser, pos, line
                int.show swp -- ..., parser, "line, pos
                stack_ast_node.init -- array, i, node
                STACK_AST_EXPR_NUMBER -- array, i, node, kind
                swp stack_ast_node.& -- array, i, kind, ptr
                stack_ast_expr.init -- array, i, expr
                rot' dup2 array.delete unwrap rot -- array, i, expr
                dup3 -- ..., expr, array, i, expr
                stack_ast_expr.& array.insert unwrap pop -- array, i
            else dup STACK_SPECIAL_COL string.= if
                pop -- ..., node
                dup -- ..., node, node
                stack_ast_node.parser -- ..., node, parser
                dup stack_parser.lexer -- ..., node, parser, lexer
                rot stack_ast_node.pos -- ..., parser, lexer, pos
                dup rot swp  -- ..., parser, pos, lexer, pos
                stack_lexer.pos.to_lc swp pop -- ..., parser, pos, line
                int.show swp -- ..., parser, "line, pos
                stack_ast_node.init -- array, i, node
                STACK_AST_EXPR_NUMBER -- array, i, node, kind
                swp stack_ast_node.& -- array, i, kind, ptr
                stack_ast_expr.init -- array, i, expr
                rot' dup2 array.delete unwrap rot -- array, i, expr
                dup3 -- ..., expr, array, i, expr
                stack_ast_expr.& array.insert unwrap pop -- array, i
            else
                pop2
            fi fi fi -- arr, i
        else dup STACK_AST_EXPR_COND = if
            pop stack_ast_expr.expr stack_ast_cond.* -- ..., cond

            dup stack_ast_cond.if_ -- ..., cond, if_
            0 swp stack_preprocessor.run.expand.special.exprs -- ..., cond

            dup stack_ast_cond.else_ -- ..., cond, else_
            0 swp stack_preprocessor.run.expand.special.exprs -- ..., cond

            pop -- ...
        else
            pop2
        fi fi -- arr, i

        1 + swp stack_preprocessor.run.expand.special.exprs -- ()
    else
        pop pop
    fi -- ()
end

func stack_preprocessor.run.expand.special (int, stack_preprocessor, stack_ast) () in -- i, pre, ast
    rot swp -- pre, i, ast
    dup stack_ast.features dup array.count rot4 dup rot < if -- pre ast array<feat> i
        dup2 array.get unwrap stack_ast_feature.* dup stack_ast_feature.kind rot4 pop -- pre, ast, i, feat, kind
        dup STACK_AST_FEATURE_FUNC = if -- pre, ast, i, feat, kind
            pop stack_ast_feature.feature -- pre, ast, i, ptr
            stack_ast_func.* -- pre, ast, i, func'
            stack_ast_func.exprs -- pre, ast, i, exprs'

            0 swp -- pre, ast, i, 0, exprs'
            stack_preprocessor.run.expand.special.exprs -- pre, ast, i
            rot'
        else
            pop2 rot'
        fi -- i, pre, ast

        rot 1 + rot' stack_preprocessor.run.expand.special
    else
        pop pop pop pop
    fi -- ()
end

func stack_preprocessor.run' (stack_preprocessor, stack_ast) () in -- pre, ast
    -- TODO: generate data code consts init getters setters
    -- TODO: generate consts for int ptr bool

    dup2 0 rot' stack_preprocessor.run.expand.consts -- pre, ast
    dup2 0 rot' stack_preprocessor.run.expand.funcs -- pre, ast

    dup2 0 rot' stack_preprocessor.run.expand.special -- pre, ast

    pop2
end

func stack_preprocessor.run (stack_preprocessor, stack_ast) () in -- pre, ast
    dup2 0 rot' stack_preprocessor.run.import -- pre, ast

    dup2 stack_preprocessor.run.base.consts -- pre, ast

    dup2 stack_preprocessor.run' -- pre, ast

    pop2
end

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
    unwrap
end

func stack_assembler.func.name' (int, string, array) (int) in -- i, name, array<string>
    dup array.count -- i, name, array<string>, c
    rot4 dup rot -- name, array<string>, i, i, c
    >= if -- name, array<string>, i
        rot' swp string.& array.append unwrap -- i
    else
        dup2 array.get unwrap -- name, array<string>, i, ptr
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
        rot' swp stack_literal.& array.append unwrap -- i
    else
        dup2 array.get unwrap -- stack_literal, array<string>, i, ptr
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
    dup stack_ast_node.value rot dup rot stack_assembler.func.name "    call    " swp string.concat -- name, asm, str
    " ; " string.concat  -- name, asm, str
    rot dup stack_ast_node.value rot swp string.concat swp rot' -- name, asm, str

    swp dup rot emit

    pop2
end

func stack_assembler.emit.expr.cond (stack_assembler, stack_ast_cond) () in -- asm, cond
    swp dup stack_assembler.if_counter dup 1 + swp 0 + rot' dup2 stack_assembler.if_counter.set pop -- cond, i, asm

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

    pop3
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
        dup2 array.get unwrap -- asm, array<expr>, i, ptr
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

func stack_assembler.emit.feature (stack_assembler, stack_ast_feature) () in -- asm, feature
    dup stack_ast_feature.kind
    dup STACK_AST_FEATURE_DATA = if -- asm, feat, kind
        pop3 -- TODO: data.* and data.&
    else dup STACK_AST_FEATURE_FUNC = if
        pop stack_ast_feature.feature stack_ast_func.* stack_assembler.emit.func
    else dup STACK_AST_FEATURE_CONST = if
        pop3 -- nothing to do for const in asm
    else dup STACK_AST_FEATURE_IMPORT = if
        pop3 -- nothing to do for import in asm
    else
        todo pop3
    fi fi fi fi -- ()
end

func stack_assembler.emit.ast.features' (int, stack_assembler, array) () in -- i, asm, array<stack_ast_feature>
    dup array.count -- i, asm, array<feature>, c
    rot4 dup rot -- asm, array<feature>, i, i, c
    >= if -- asm, array<feature>, i
        pop3
    else
        dup2 array.get unwrap -- asm, array<feature>, i, ptr
        stack_ast_feature.* -- asm, array<feature>, i, feature
        rot4 dup rot stack_assembler.emit.feature rot' -- asm, array<feature>, i
        1 + rot' -- i+1, asm, array<feature>
        stack_assembler.emit.ast.features' -- ()
    fi -- ()
end

func stack_assembler.emit.ast.features (stack_assembler, array) () in 0 rot' stack_assembler.emit.ast.features' end

func stack_assembler.emit.ast (stack_assembler, stack_ast) () in -- asm, prog
    swp
    dup "section '.text' executable" emit
    dup ""                           emit

    dup2 swp stack_ast.features stack_assembler.emit.ast.features

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
        dup2 array.get unwrap -- asm, array<stack_literal>, i, ptr
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

data stack_args
    ( string filename
    , bool lexer
    , bool parser
    , bool preprocessor
    , bool assembler
    )

func stack_args.init.empty () (stack_args) in
    "" false false false false stack_args.init
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
    "  -p, --parser\n" string.stdout
    "      flag to stop on the parser phase\n" string.stdout
    "\n" string.stdout
    "  -P, --preprocessor\n" string.stdout
    "      flag to stop on the preprocessor phase\n" string.stdout
    "\n" string.stdout
    "  -a, --assembler\n" string.stdout
    "      flag to stop on the assembler phase\n" string.stdout
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
        swp dup2 array.get unwrap ptr.* dup ptr.strlen swp string.init -- args, array, i, string

        dup "--help" string.= swp dup "-h" string.= rot or if -- args, array, i, string
            pop stack_args.usage 0 sys.exit
        else dup "--lexer" string.= swp dup "-l" string.= rot or if
            rot4 dup true stack_args.lexer.set rot4' pop
        else dup "--parser" string.= swp dup "-p" string.= rot or if
            rot4 dup true stack_args.parser.set rot4' pop
        else dup "--preprocessor" string.= swp dup "-P" string.= rot or if
            rot4 dup true stack_args.preprocessor.set rot4' pop
        else dup "--assembler" string.= swp dup "-a" string.= rot or if
            rot4 dup true stack_args.assembler.set rot4' pop
        else
            -- array, i, string args L
            rot4 dup stack_args.filename string.len 0 > if -- array, i, string, args
                rot4' stack_args.usage
                "Got unexpected CLI argument: `" swp string.concat "`\n" string.concat string.stderr 1 sys.exit
            else
                dup rot stack_args.filename.set rot'
            fi -- args, array, i
        fi fi fi fi fi -- args, array, i

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
        pop STDIN
    else
        "r" stdlib.fopen unwrap
    fi dup -- args, fd, fd

    stdlib.fread.<eof> unwrap -- args, fd, string
    swp stdlib.fclose unwrap -- args, string
    swp dup stack_args.filename -- string, args, fname
    dup string.len 0 = if pop "stdin" fi -- string, args, fname
    swp rot' -- args, string, filename
    stack_lexer.init.with_buffer -- args, stack_lexer

    swp dup stack_args.lexer if  -- stack_lexer, args
        swp dup stack_lexer.dump -- args, stack_lexer
        0 sys.exit swp
    fi swp -- args, stack_lexer

    stack_parser.init.with_lexer -- args, fd, stack_parser
    stack_parser.parse -- args, fd, stack_ast, ok
    unwrap -- args, fd, stack_ast

    swp dup stack_args.parser if -- stack_ast, args
        swp dup stack_ast.dump -- args, stack_lexer
        0 sys.exit swp
    fi swp -- args, stack_ast

    stack_preprocessor.init -- args, ast, pre
    dup2 swp stack_preprocessor.run pop -- args, ast

    swp dup stack_args.preprocessor if -- stack_ast, args
        swp dup stack_ast.dump -- args, stack_lexer
        0 sys.exit swp
    fi swp -- args, stack_ast

    -- TODO: typecheck stage

    STDOUT stack_assembler.init.with_fd -- args, stack_ast, asm
    swp stack_assembler.emit -- args

    dup stack_args.assembler if -- args
        0 sys.exit
    fi -- args

    -- TODO: fasm stage
    -- TODO: ld stage

    pop

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
