-- This is the standard library of `stack`.

const INT_SIZE 8

const STDIN 0
const STDOUT 1
const STDERR 2

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

const ptr.sizeof INT_SIZE

func ptr.alloc (int) (ptr) extern   -- allocate n bytes in memory
func ptr.+ (ptr, int) (ptr) extern  -- offset address
func ptr.& (ptr) (ptr) extern       -- pointer ref (data.ptr)
func ptr.* (ptr) (ptr) extern       -- pointer deref (ptr.data)
func ptr.@ (ptr, ptr) () extern     -- (dst, src) copy one byte

func ptr.realloc (ptr, int, int) (ptr) in -- src, osz, sz
    dup2 > if -- src, osz, sz
        pop2
    else
        ptr.alloc -- src, osz, dst
        rot' -- dst, src, osz
        ptr.memcpy -- dst
    fi -- ptr
end

func ptr.memcpy' (ptr, ptr, ptr, int) (ptr) in -- ret, dst, src, sz
    dup 0 <= if -- ret, dst, src, sz
        pop pop pop -- ret
    else
        rot' -- ret, sz, dst, src
        dup2 -- ret, sz, dst, src, dst, src
        ptr.@ -- ret, sz, dst, src
        1 ptr.+ -- ret, sz, dst, src+1
        swp 1 ptr.+ swp -- ret, sz, dst+1, src+1
        rot 1 - -- ret, dst+1, src+1, sz-1
        ptr.memcpy' -- ret
    fi
end

func ptr.memcpy (ptr, ptr, int) (ptr) in -- dst, src, sz
    rot dup rot4' rot4' ptr.memcpy'
end

func ptr.memcmp' (ptr, ptr, int) (int) in -- s1, s2, n
    dup 0 <= if -- s1, s2, n
        pop pop pop 0
    else
        rot' -- n, s1, s2

        dup2 -- n, s1, s2, s1, s2
        byte.init swp byte.init swp -- n, s1, s2, b1, b2
        - -- n, s1, s2, b1-b2
        dup 0 = if -- n, s1, s2, b1-b2
            pop -- n, s1, s2
            1 ptr.+ -- n, s1, s2+1
            swp 1 ptr.+ swp -- n, s1+1, s2+1
            rot 1 - -- s1+1, s2+1, n-1
            ptr.memcmp' -- int
        else
            rot4' pop3 -- b1-b2
        fi
    fi
end

func ptr.memcmp (ptr, ptr, int) (int) in -- s1, s2, n
    ptr.memcmp'
end

-- DATA INT
data int extern

const int.sizeof INT_SIZE

func + (int, int) (int) extern
func - (int, int) (int) extern
func * (int, int) (int) extern
func / (int, int) (int) extern
func % (int, int) (int) extern
func | (int, int) (int) extern
func & (int, int) (int) extern
func ^ (int, int) (int) extern
func >> (int, int) (int) extern
func << (int, int) (int) extern

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

-- byte is a subdata of int

func byte.isdigit (int) (bool) in -- chr
    dup 48 >= swp 57 <= and
end

func byte.init (ptr) (int) in -- chr*
    int.sizeof ptr.alloc -- str, ptr
    dup rot' swp 1 ptr.memcpy pop -- ptr
    ptr.int -- byte
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

-- compute how much memory you need to allocate for the string ptr
-- taking into account memory alignment by 8
func string.memory-needed (int) (int) in -- sz
    7 + 8 / 8 *
end

const string.sizeof int.sizeof ptr.sizeof +

const string.len.offset 0
const string.str.offset string.len.offset int.sizeof +

func string.init (int, ptr) (string) in
    string.sizeof ptr.alloc -- L, str, ptr

    dup rot' -- L, ptr, str, ptr
    string.str.offset ptr.+ -- L, ptr, str, ptr+
    swp ptr.& ptr.sizeof -- L, ptr, ptr+, &str, sz
    ptr.memcpy pop -- L, ptr

    dup rot' -- ptr, L, ptr
    string.len.offset ptr.+ -- ptr, L, ptr+
    swp int.ptr int.sizeof -- ptr, ptr+, &L, sz
    ptr.memcpy pop -- ptr

    ptr.string -- string
end

func string.len (string) (int) in
    string.ptr string.len.offset ptr.+ ptr.int
end

func string.str (string) (ptr) in
    string.ptr string.str.offset ptr.+ ptr.*
end

func string.concat (string, string) (string) in -- s1, s2
    dup2 string.len swp string.len + -- s1, s2, L
    dup rot4' string.memory-needed ptr.alloc -- L, s1, s2, ptr
    dup rot4' -- L, ptr, s1, s2, ptr
    rot dup string.str swp string.len dup rot4' ptr.memcpy -- L, ptr, s2, L1, ptr
    swp ptr.+ -- L, ptr, s2, ptr+L1
    swp dup string.str swp string.len ptr.memcpy -- L, ptr, ptr+L1
    pop -- L, ptr
    string.init
end

func string.substr (string, int, int) (string) in -- s, i, n
    rot -- i, n, s
    string.str -- i, n, str
    rot ptr.+ swp -- str+i, n
    dup string.memory-needed ptr.alloc -- str+i, n, ptr
    dup rot4' -- ptr, str+i, n, ptr
    rot' -- ptr, ptr, str+i, n
    dup rot4' -- ptr, n, ptr, str+i, n
    ptr.memcpy pop -- ptr, n
    swp -- n, ptr
    string.init
end

func string.!! (string, int) (int) in -- s, i
    swp -- i, s
    string.str swp ptr.+ -- str+i
    byte.init -- byte
end

func string.= (string, string) (bool) in -- s1, s2
    dup2 string.len swp string.len swp -- s1, s2, L1, L2
    dup rot' = if -- s1, s2, L
        rot' string.str swp string.str swp rot -- s1.str, s2.str, L
        ptr.memcmp -- int
        0 = -- bool
    else
        pop3 false
    fi -- bool
end

func string.stdin () (string) in -- ()
    STDIN stdlib.fread.<eof> -- s, ok
    not if panic fi -- s
end

func string.stdout (string) () in -- s
    STDOUT swp stdlib.fwrite -- bool
    not if panic fi
end

func string.stderr (string) () in -- s
    STDERR swp stdlib.fwrite -- bool
    not if panic fi
end

-- SYSCALLS

func syscall1 (a, int) (int) extern
func syscall3 (a, b, c, int) (int) extern

func sys.read (int, ptr, int) (int) in 0 syscall3 end
func sys.write (int, ptr, int) (int) in 1 syscall3 end
func sys.open (ptr, int, int) (int) in 2 syscall3 end
func sys.close (int) (int) in 3 syscall1 end
func sys.exit (int) () in 60 syscall1 pop end

const O_RDONLY 0
const O_WRONLY 1
const O_RDWR   2
const O_CREAT  64

const O_0644   420

-- STDLIB

const stdlib.MAX_LINE 1024

func abort () () in
    1 sys.exit
end

func panic () () in
    "panic" STDERR swp stdlib.fwrite pop abort
end

func stdlib.fopen (string, string) (int, bool) in -- fn, md
    swp string.str swp
    dup "r" string.= if -- fn, md
        pop O_RDONLY O_0644 sys.open true
    else dup "w" string.= if -- fn, md
        pop O_WRONLY O_CREAT | O_0644 sys.open true
    else
        pop2 0 false
    fi fi -- int, ok
end

func stdlib.fclose (int) (bool) in -- fd
    sys.close 0 =
end

func stdlib.fread (int, int) (string, bool) in -- fd, L
    dup string.memory-needed ptr.alloc -- fd, L, ptr
    dup rot4' -- ptr, fd, L, ptr
    swp -- ptr, fd, ptr, L
    sys.read -- ptr, L
    dup -- ptr, L, L
    0 < if -- ptr, L
        pop2 "" false
    else
        swp string.init true
    fi -- s, ok
end


func stdlib.fread.<eof> (int) (string, bool) in -- fd
    dup stdlib.MAX_LINE stdlib.fread -- fd, s, ok
    not if -- fd, s
        pop2 "" false
    else
        dup string.len -- fd, s, L
        dup 0 <= if -- fd, s, L
            pop3 "" false
        else stdlib.MAX_LINE >= if -- fd, s
            swp stdlib.fread.<eof> -- s1, s2, ok
            not if -- s1, s2
                pop2 "" false
            else
                string.concat true
            fi -- s, ok
        else -- fd, s
            swp pop true
        fi fi -- s, ok
    fi -- s, ok
end

func stdlib.fwrite (int, string) (bool) in -- fd, s
    dup string.str -- fd, s, ptr
    swp string.len -- fd, ptr, L
    sys.write -- int
    0 >= -- bool
end

-- DATA ARRAY
data array (int capacity, int count, int sz, ptr items)

const array.SIZE 1024

const array.sizeof int.sizeof int.sizeof int.sizeof ptr.sizeof + + +
const array.capacity.offset 0
const array.count.offset array.capacity.offset int.sizeof +
const array.sz.offset array.count.offset int.sizeof +
const array.items.offset array.sz.offset int.sizeof +

func array.init (int) (array) in -- sz
    0 0 0 int.ptr -- sz, C, L, xs
    rot4 swp -- C, L, sz, xs
    array.sizeof ptr.alloc

    dup rot' array.items.offset ptr.+ swp ptr.& ptr.sizeof ptr.memcpy pop
    dup rot' array.sz.offset ptr.+ swp int.ptr int.sizeof ptr.memcpy pop
    dup rot' array.count.offset ptr.+ swp int.ptr int.sizeof ptr.memcpy pop
    dup rot' array.capacity.offset ptr.+ swp int.ptr int.sizeof ptr.memcpy pop

    ptr.array -- array
end

func array.capacity (array) (int) in -- a
    array.ptr array.capacity.offset ptr.+ ptr.int
end

func array.count (array) (int) in -- a
    array.ptr array.count.offset ptr.+ ptr.int
end

func array.sz (array) (int) in -- a
    array.ptr array.sz.offset ptr.+ ptr.int
end

func array.items (array) (ptr) in -- a
    array.ptr array.items.offset ptr.+ ptr.*
end

func array.capacity.set (array, int) () in -- a, C
    swp array.ptr array.capacity.offset ptr.+ swp int.ptr int.sizeof ptr.memcpy pop
end

func array.count.set (array, int) () in -- a, L
    swp array.ptr array.count.offset ptr.+ swp int.ptr int.sizeof ptr.memcpy pop
end

func array.sz.set (array, int) () in -- a, sz
    swp array.ptr array.sz.offset ptr.+ swp int.ptr int.sizeof ptr.memcpy pop
end

func array.items.set (array, ptr) () in -- a, items
    swp array.ptr array.items.offset ptr.+ swp ptr.& ptr.sizeof ptr.memcpy pop
end

func array.get (array, int) (ptr, bool) in -- a, i
    dup -- a, i, i
    rot -- i, i, a
    dup array.count -- i, i, a, L
    rot <= if -- i, a
        pop2 0 int.ptr false
    else
        dup array.sz -- i, a, sz
        rot * -- a, i*sz
        swp -- i*sz, a
        array.items -- i*sz, ptr
        swp ptr.+ -- ptr+i*sz
        true
    fi -- ptr, ok
end

func array.append (array, ptr) (bool) in -- a, ptr
    swp -- ptr, a
    dup array.count -- ptr, a, L
    swp dup array.capacity -- ptr, L, a, C
    rot <= if -- ptr, a
        dup array.capacity -- ptr, a, C
        dup 2 * -- ptr, a, C, C*2
        dup 0 <= if -- ptr, a, C, C*2
            pop array.SIZE -- ptr, a, int
        fi -- ptr, a, C, nC

        rot swp dup2 -- ptr, C, a, nC, a, nC
        array.capacity.set -- ptr, C, a, nC

        swp dup array.sz -- ptr, C, nC, a, sz
        rot4 * -- ptr, nC, a, sz*C
        swp dup array.sz -- ptr, nC, sz*C, a, sz
        rot4 * -- ptr, sz*C, a, sz*nC
        swp dup array.items -- ptr, sz*C, sz*nC, a, a.ptr
        rot4 rot4 -- ptr, a, a.ptr, sz*C, sz*nC
        ptr.realloc -- ptr, a, a.ptr'
        swp dup rot -- ptr, a, a, a.ptr'
        array.items.set -- ptr, a
    fi -- ptr, a

    dup array.count -- ptr, a, L
    dup2 -- ptr, a, L, a, L
    swp dup array.sz -- ptr, a, L, L, a, sz
    rot * -- ptr, a, L, a, L*sz
    swp array.items swp ptr.+ -- ptr, a, L, xs+L*sz
    rot4 rot4 -- L, xs+L*sz, ptr, a
    dup array.sz swp rot4' -- L, a, xs+L*sz, ptr, sz
    ptr.memcpy pop -- L, a
    swp -- a, L
    1 + -- a, L+1
    array.count.set true -- bool
end
