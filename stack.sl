@import stdlib

data stack_ast_node (string value)

const stack_ast_node.sizeof string.sizeof

const stack_ast_node.value.offset 0

func stack_ast_node.init (string) (stack_ast_node) in -- s
    stack_ast_node.sizeof ptr.alloc -- s, ptr

    dup rot' ptr.int stack_ast_node.value.offset + int.ptr swp ...

    ptr.stack_ast_node
end

func stack_ast_node.value (stack_ast_node) (string) in -- a
    stack_ast_node.ptr -- ptr
    ptr.int stack_ast_node.value.offset + int.ptr -- ptr+
    ptr.@int -- int
end

data stack_ast_expr (string number)
data stack_ast_func (stack_ast_node name, array exprs)
data stack_ast_prog (array funcs)

const stack_ast_prog.sizeof 0

func stack_ast_prog.init () (stack_ast_prog) in -- fd
    stack_ast_prog.sizeof ptr.alloc -- ptr

    ptr.stack_ast_prog
end

data stack_assembler (int fd)

const stack_assembler.sizeof 8
const stack_assembler.fd.offset 0

func stack_assembler.init (int) (stack_assembler) in -- fd
    stack_assembler.sizeof ptr.alloc -- fd, ptr

    dup rot' ptr.int stack_assembler.fd.offset + int.ptr swp int.ptr ptr.int ptr.!int -- ptr

    ptr.stack_assembler
end

func stack_assembler.fd (stack_assembler) (int) in -- asm
    stack_assembler.ptr -- ptr
    ptr.int stack_assembler.fd.offset + int.ptr -- ptr+0
    ptr.@int -- int
end

func emit (stack_assembler, string) () in -- asm, s
    swp stack_assembler.fd swp -- fd, s
    "\n" string.concat -- fd, s
    stdlib.fwrite
    not if panic fi
end

func stack_assembler.emit.allocator (stack_assembler) () in -- asm
    dup "section '.data' writeable"             emit
    dup ""                                      emit
    dup "stack_pos dq 0"                        emit
    dup "stack_end dq 0"                        emit
    dup "heap_pos dq 0"                         emit
    dup "heap_end dq 0"                         emit
    dup ""                                      emit
    dup "section '.text' executable"            emit
    dup ""                                      emit
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
    dup "; stack push" emit
    dup ";" emit
    dup ";   INPUT: rdi contains the int64 (pointer) that we add to the stack" emit
    dup ";   OUTPUT: nothing" emit
    dup ";" emit
    dup "stack_push:" emit
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
    dup ";" emit
    dup ";" emit
    dup "; memcpy" emit
    dup ";   INPUT:" emit
    dup ";       rdi points to destination" emit
    dup ";       rsi points to source" emit
    dup ";       rdx contains the number of bytes to copy" emit
    dup ";   STACK: empty" emit
    dup ";   OUTPUT: nothing" emit
    dup ";" emit
    dup "memcpy:" emit
    dup "    push    rbp                        ; save return address" emit
    dup "    mov     rbp, rsp                   ; set up stack frame" emit
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
    dup "    pop     rbp                        ; restore return address" emit
    dup "    ret" emit
    dup "" emit

    pop
end

func stack_assembler.emit.entry (stack_assembler) () in -- asm
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
    dup "    call   func.1 ; main" emit
    dup "" emit
    dup "    ; Exit the program" emit
    dup "    call    stack_pop" emit
    dup "    mov     rdi, rax" emit
    dup "    mov     rax, 60" emit
    dup "    syscall" emit
    dup "" emit

    pop
end

func stack_assembler.emit.ast (stack_assembler, stack_ast_prog) () in -- asm, ast
    swp
    dup "section '.text' executable" emit
    dup "" emit

end

func stack_assembler.emit (stack_assembler, stack_ast_prog) () in -- asm, ast
    swp
    dup "format ELF64" emit
    dup "" emit

    dup stack_assembler.emit.allocator
    dup stack_assembler.emit.entry
    swp dup2 stack_assembler.emit.ast

    pop2
end

func main () (int) in
    STDOUT stack_assembler.init -- asm

    stack_ast_func.sizeof array.init dup -- a, a
    "main" stack_ast_node.init -- a, a, node
    stack_ast_expr.sizeof array.init -- a, a, node, a'
    dup "0" stack_ast_expr.init stack_ast_expr.ptr -- a, a, node, a', a', expr
    array.append -- a, a, node, a', bool
    not if panic fi -- a, a, node, a'
    stack_ast_func.init -- a, a, fn
    array.append -- a, bool
    not if panic fi -- a
    stack_ast_prog.init -- asm, ast

    stack_assembler.emit -- ()

    0
end
