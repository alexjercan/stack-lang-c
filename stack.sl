@import stdlib

data stack_ast_node (string value)

const stack_ast_node.sizeof string.sizeof

const stack_ast_node.value.offset 0

func stack_ast_node.init (string) (stack_ast_node) in -- s
    stack_ast_node.sizeof ptr.alloc -- s, ptr

    dup rot' -- ptr, s, ptr
    stack_ast_node.value.offset ptr.+ -- ptr, s, ptr+
    swp string.ptr string.sizeof -- ptr, ptr+, &str, sz
    ptr.memcpy pop -- ptr

    ptr.stack_ast_node -- stack_ast_node
end

func stack_ast_node.value (stack_ast_node) (string) in -- a
    stack_ast_node.ptr stack_ast_node.value.offset ptr.+ ptr.string -- string
end

data stack_ast_expr (string number)

const stack_ast_expr.sizeof string.sizeof

const stack_ast_expr.number.offset 0

func stack_ast_expr.init (string) (stack_ast_expr) in -- s
    stack_ast_expr.sizeof ptr.alloc -- s, ptr

    dup rot' -- ptr, s, ptr
    stack_ast_expr.number.offset ptr.+ -- ptr, s, ptr+
    swp string.ptr string.sizeof -- ptr, ptr+, &str, sz
    ptr.memcpy pop -- ptr

    ptr.stack_ast_expr -- stack_ast_expr
end

func stack_ast_expr.number (stack_ast_expr) (string) in -- a
    stack_ast_expr.ptr stack_ast_expr.number.offset ptr.+ ptr.string -- string
end

data stack_ast_func (stack_ast_node name, array exprs)

const stack_ast_func.sizeof stack_ast_node.sizeof array.sizeof +

const stack_ast_func.name.offset 0
const stack_ast_func.exprs.offset stack_ast_func.name.offset stack_ast_node.sizeof +

func stack_ast_func.init (stack_ast_node, array) (stack_ast_func) in -- n, a
    stack_ast_func.sizeof ptr.alloc -- n, a, ptr

    dup rot' -- n, ptr, a, ptr
    stack_ast_func.exprs.offset ptr.+ -- n, ptr, a, ptr+
    swp array.ptr array.sizeof -- n, ptr, ptr+, &a, sz
    ptr.memcpy pop -- n, ptr

    dup rot' -- ptr, n, ptr
    stack_ast_func.name.offset ptr.+ -- ptr, n, ptr+
    swp stack_ast_node.ptr stack_ast_node.sizeof -- ptr, ptr+, &n, sz
    ptr.memcpy pop -- ptr

    ptr.stack_ast_func -- stack_ast_func
end

func stack_ast_func.name (stack_ast_func) (stack_ast_node) in -- a
    stack_ast_func.ptr stack_ast_func.name.offset ptr.+ ptr.stack_ast_node -- stack_ast_node
end

func stack_ast_func.exprs (stack_ast_func) (array) in -- a
    stack_ast_func.ptr stack_ast_func.exprs.offset ptr.+ ptr.array -- array
end

data stack_ast_prog (array funcs)

const stack_ast_prog.sizeof array.sizeof

const stack_ast_prog.funcs.offset 0

func stack_ast_prog.init (array) (stack_ast_prog) in -- s
    stack_ast_prog.sizeof ptr.alloc -- s, ptr

    dup rot' -- ptr, s, ptr
    stack_ast_prog.funcs.offset ptr.+ -- ptr, s, ptr+
    swp array.ptr array.sizeof -- ptr, ptr+, &str, sz
    ptr.memcpy pop -- ptr

    ptr.stack_ast_prog -- stack_ast_prog
end

func stack_ast_prog.funcs (stack_ast_prog) (array) in -- a
    stack_ast_prog.ptr stack_ast_prog.funcs.offset ptr.+ ptr.array -- array
end

data stack_assembler (int fd, array func_map)

const stack_assembler.sizeof int.sizeof array.sizeof +

const stack_assembler.fd.offset 0
const stack_assembler.func_map.offset stack_assembler.fd.offset int.sizeof +

func stack_assembler.init (int) (stack_assembler) in -- s
    string.sizeof array.init swp -- array<string>, s
    stack_assembler.sizeof ptr.alloc -- a, s, ptr

    dup rot' -- ..., ptr, s, ptr
    stack_assembler.fd.offset ptr.+ -- ..., ptr, s, ptr+
    swp int.ptr int.sizeof -- ..., ptr, ptr+, &str, sz
    ptr.memcpy pop -- array<string>, ptr

    dup rot' -- ptr, array<string>, ptr
    stack_assembler.func_map.offset ptr.+ -- ptr, array<string>, ptr+
    swp array.ptr array.sizeof -- ptr, ptr+, &array<string>, sz
    ptr.memcpy pop -- ptr

    ptr.stack_assembler -- stack_assembler
end

func stack_assembler.fd (stack_assembler) (int) in -- a
    stack_assembler.ptr stack_assembler.fd.offset ptr.+ ptr.int -- int
end

func stack_assembler.func_map (stack_assembler) (array) in -- a
    stack_assembler.ptr stack_assembler.func_map.offset ptr.+ ptr.array -- array
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
        rot' swp string.ptr array.append not if panic fi -- i
    else
        dup2 array.get not if panic fi -- name, array<string>, i, ptr
        ptr.string -- name, array<string>, i, string
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

func stack_assembler.emit.expr (stack_assembler, stack_ast_expr) () in -- asm, expr
    -- TODO: use constants and stuff like that like in the real comp
    dup stack_ast_expr.number "    mov     rdi, " swp string.concat rot' -- string, asm, expr

    swp
    dup rot4 emit
    dup "    call    stack_push" emit

    pop2
end

func stack_assembler.emit.exprs' (int, stack_assembler, array) () in -- asm, array<expr>
    dup array.count -- i, asm, array<expr>, c
    rot4 dup rot -- asm, array<expr>, i, i, c
    >= if -- asm, array<expr>, i
        pop3
    else
        dup2 array.get not if panic fi -- asm, array<expr>, i, ptr
        ptr.stack_ast_expr -- asm, array<expr>, i, expr
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

func stack_assembler.emit.ast.funcs' (int, stack_assembler, array) () in -- i, a
    dup array.count -- i, asm, array<func>, c
    rot4 dup rot -- asm, array<func>, i, i, c
    >= if -- asm, array<func>, i
        pop3
    else
        dup2 array.get not if panic fi -- asm, array<func>, i, ptr
        ptr.stack_ast_func -- asm, array<func>, i, func
        rot4 dup rot stack_assembler.emit.func rot' -- asm, array<func>, i
        1 + rot' -- i+1, asm, array<func>
        stack_assembler.emit.ast.funcs' -- ()
    fi -- ()
end

func stack_assembler.emit.ast.funcs (stack_assembler, array) () in 0 rot' stack_assembler.emit.ast.funcs' end

func stack_assembler.emit.ast (stack_assembler, stack_ast_prog) () in -- asm, prog
    swp
    dup "section '.text' executable" emit
    dup ""                           emit

    dup2 swp stack_ast_prog.funcs stack_assembler.emit.ast.funcs

    pop2
end

func stack_assembler.emit (stack_assembler, stack_ast_prog) () in -- asm, ast
    swp
    dup "format ELF64" emit
    dup ""             emit

    dup stack_assembler.emit.allocator
    dup stack_assembler.emit.entry
    dup stack_assembler.emit.keywords
    swp dup2 stack_assembler.emit.ast

    pop2
end

func main () (int) in
    STDOUT stack_assembler.init -- asm

    -- asm
    STACK_FUNC_MAIN stack_ast_node.init -- node
    "69" stack_ast_expr.init -- expr
    stack_ast_expr.sizeof array.init dup -- node, expr, array<expr>, array<expr>
    rot stack_ast_expr.ptr -- node, array<expr>, array<expr>, &expr
    array.append -- node, array<expr>, ok
    not if panic fi -- node, array<expr>
    stack_ast_func.init -- func
    stack_ast_func.sizeof array.init dup -- func, array<func>, array<func>
    rot stack_ast_func.ptr -- array<func>, array<func>, &func
    array.append -- array<func>, ok
    not if panic fi -- array<func>
    stack_ast_prog.init -- ast

    -- asm, ast
    stack_assembler.emit -- ()

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
const STACK_FUNC_PTR_REF "ptr.&"
const STACK_FUNC_PTR_DEREF "ptr.*"
const STACK_FUNC_PTR_COPY8 "ptr.@"

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
    -- PTR REF
    dup ";" emit
    dup ";" emit
    dup "; memory ref" emit
    dup ";" emit
    dup ";   INPUT: (ptr)" emit
    dup ";   OUTPUT: (ptr)" emit
    dup dup STACK_FUNC_PTR_REF dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 24                    ; allocate 3 local variables" emit
    dup "" emit
    dup "; ref ptr" emit
    dup "    call    stack_pop_addr" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push" emit
    dup "" emit
    dup "    add     rsp, 24                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- PTR DEREF
    dup ";" emit
    dup ";" emit
    dup "; memory deref" emit
    dup ";" emit
    dup ";   INPUT: (ptr)" emit
    dup ";   OUTPUT: (ptr)" emit
    dup dup STACK_FUNC_PTR_DEREF dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 24                    ; allocate 3 local variables" emit
    dup "" emit
    dup "; deref ptr" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    call    stack_push_addr" emit
    dup "" emit
    dup "    add     rsp, 24                    ; deallocate local variables" emit
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit
    -- PTR COPY 8
    dup ";" emit
    dup ";" emit
    dup "; memory copy byte" emit
    dup ";" emit
    dup ";   INPUT: (dst, src)" emit
    dup ";   OUTPUT: ()" emit
    dup dup STACK_FUNC_PTR_COPY8 dup rot' stack_assembler.func.name ": ; " string.concat swp string.concat emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
    dup "    sub     rsp, 24                    ; allocate 3 local variables" emit
    dup "" emit
    dup "; copy one byte" emit
    dup "    ; t0 <- src" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_0], rax" emit
    dup "" emit
    dup "    ; t1 <- dst" emit
    dup "    call    stack_pop" emit
    dup "    mov     qword [rbp - loc_1], rax" emit
    dup "" emit
    dup "    ; copy byte *dst = *src" emit
    dup "    mov     rdi, qword [rbp - loc_1]" emit
    dup "    mov     rsi, qword [rbp - loc_0]" emit
    dup "    mov     al, byte [rsi]" emit
    dup "    mov     byte [rdi], al" emit
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
