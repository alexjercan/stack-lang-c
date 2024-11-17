-- This is the standard library of `stack`.

const INT_SIZE 8

-- BASE FUNCTIONS

func dup (a) (a, a) extern                      -- duplicate the first item
func swp (a, b) (b, a) extern                   -- swap the first two items
func rot (a, b, c) (b, c, a) extern             -- rotate the first 3 items
func rot4 (a, b, c, d) (b, c, d, a) extern      -- rotate the first 4 items
func pop (a) () extern                          -- discard the first item

-- duplicate first 2 elements of the stack
func dup2 (a, b) (a, b, a, b) in
    swp dup rot dup rot'
end

-- pop the first 2 elements of the stack
func pop2 (a, b) () in
    pop pop
end

-- duplicate first 3 elements of the sack
func dup3 (a, b, c) (a, b, c, a, b, c) in
    rot dup rot4'
    rot dup rot4'
    rot dup rot4'
end

-- pop the first 3 eleemnts of the stack
func pop3 (a, b, c) () in
    pop pop pop
end

-- rotate 3 elements counter clockwise
func rot' (a, b, c) (c, a, b) in rot rot end

-- rotate 4 elements counter clockwise
func rot4' (a, b, c, d) (d, a, b, c) in rot4 rot4 rot4 end

-- DATA PTR
data ptr extern

func ptr.alloc (int) (ptr) extern       -- allocate n bytes in memory
func ptr.!int (ptr, int) () extern      -- copy int into memory
func ptr.@int (ptr) (int) extern        -- copy int from memory
func ptr.!byte (ptr, int) () extern     -- copy byte into memory
func ptr.@byte (ptr) (int) extern       -- copy byte from memory

func ptr.memcpy' (ptr, ptr, ptr, int) (ptr) in -- ret, dst, src, sz
    dup 0 <= if -- ret, dst, src, sz
        pop pop pop -- ret
    else
        rot' -- ret, sz, dst, src
        dup2 ptr.@byte ptr.!byte -- ret, sz, dst, src
        ptr.int 1 + int.ptr -- ret, sz, dst, src+1
        swp ptr.int 1 + int.ptr swp -- ret, sz, dst+1, src+1
        rot 1 - -- ret, dst+1, src+1, sz-1
        ptr.memcpy' -- ret
    fi
end

func ptr.memcpy (ptr, ptr, int) (ptr) in -- dst, src, sz
    rot dup rot4' rot4' ptr.memcpy'
end

-- DATA INT
data int extern

func + (int, int) (int) extern
func - (int, int) (int) extern
func * (int, int) (int) extern
func / (int, int) (int) extern
func % (int, int) (int) extern

func abs (int) (int) in
    dup 0 < if 0 swp - fi
end

func mod (int, int) (int) in
    dup rot' % dup 0 < if + else swp pop fi
end

func digit.show (int) (string) in
    dup 0 = if pop "0" else
    dup 1 = if pop "1" else
    dup 2 = if pop "2" else
    dup 3 = if pop "3" else
    dup 4 = if pop "4" else
    dup 5 = if pop "5" else
    dup 6 = if pop "6" else
    dup 7 = if pop "7" else
    dup 8 = if pop "8" else
    dup 9 = if pop "9" else pop ""
    fi fi fi fi fi fi fi fi fi fi
end

func int.show' (int) (string) in
    dup 0 = if pop ""
    else dup 10 mod swp 10 / int.show' swp digit.show string.concat fi
end

func int.show (int) (string) in
    dup 0 = if pop "0"
    else int.show' fi
end

-- DATA BOOL
data bool extern

func > (int, int) (bool) extern
func < (int, int) (bool) extern
func = (int, int) (bool) extern

func <= (int, int) (bool) in
    dup2 < if pop2 true else = fi
end

func >= (int, int) (bool) in
    dup2 > if pop2 true else = fi
end

func not (bool) (bool) in
    if false else true fi
end

func and (bool, bool) (bool) in
    if else pop false fi
end

func or (bool, bool) (bool) in
    if pop true else fi
end

-- DATA STRING
data string (int len, ptr str)

const string.len.offset 0
const string.str.offset 8
const string.max_line 1024

const STDIN 0
const STDOUT 1
const STDERR 2

func string.len (string) (int) in
    string.ptr ptr.int string.len.offset + int.ptr ptr.@int
end

func string.str (string) (ptr) in
    string.ptr ptr.int string.str.offset + int.ptr
end

-- compute how much memory you need to allocate for the string structure
-- taking into account memory alignment by 8
func string.memory-needed (int) (int) in -- sz
    7 + 8 / 8 * 8 +
end

func string.concat (string, string) (string) in -- s1, s2
    dup2 string.len swp string.len + -- s1, s2, L
    dup rot4' string.memory-needed ptr.alloc -- L, s1, s2, ptr
    dup rot4' ptr.int string.str.offset + int.ptr -- L, ptr, s1, s2, ptr8
    rot dup string.str swp string.len dup rot4' ptr.memcpy -- L, ptr, s2, L1, ptr8
    ptr.int + int.ptr -- L, ptr, s2, ptr8+L1
    swp dup string.str swp string.len ptr.memcpy -- L, ptr, ptr8+L1
    pop dup rot -- ptr, ptr, L
    ptr.!int -- ptr
    ptr.string -- string
end

func string.substr (string, int, int) (string) in -- s, i, n
    rot -- i, n, s
    string.str -- i, n, str
    ptr.int rot + int.ptr swp -- str+i, n
    dup string.memory-needed ptr.alloc -- str+i, n, ptr
    dup rot4' ptr.int string.str.offset + int.ptr -- ptr, str+i, n, ptr8
    rot' -- ptr, ptr8, str+i, n
    dup rot4' -- ptr, n, ptr8, str+i, n
    ptr.memcpy pop -- ptr, n
    swp dup rot -- ptr, ptr, n
    ptr.!int -- ptr
    ptr.string -- string
end

func string.read (int, int) (string) in -- fd, L
    dup string.memory-needed ptr.alloc -- fd, L, ptr
    dup rot4' ptr.int string.str.offset + int.ptr -- ptr, fd, L, ptr8
    swp -- ptr, fd, ptr8, L
    sys.read -- ptr, L
    dup -- ptr, L, L
    0 < if -- ptr, L
        pop dup 0 -- ptr, ptr, 0
    else
        swp dup rot -- ptr, ptr, L
    fi
    ptr.!int -- ptr
    ptr.string -- string
end

func string.write (int, string) (int) in -- fd, s
    dup string.str -- fd, s, ptr
    swp string.len -- fd, ptr, L
    sys.write -- int
end

func string.stdin () (string) in -- ()
    STDIN string.max_line string.read
    dup string.len dup
    0 <= if sys.exit
    else string.max_line >= if string.stdin string.concat fi fi
end

func string.stdout (string) () in -- s
    STDOUT swp string.write -- int
    dup 0 < if sys.exit else pop fi
end

func string.stderr (string) () in -- s
    STDERR swp string.write -- int
    dup 0 < if sys.exit else pop fi
end

-- SYSCALLS

func syscall1 (a, int) (int) extern
func syscall3 (a, b, c, int) (int) extern

func sys.read (int, ptr, int) (int) in 0 syscall3 end
func sys.write (int, ptr, int) (int) in 1 syscall3 end
func sys.exit (int) () in 60 syscall1 pop end
