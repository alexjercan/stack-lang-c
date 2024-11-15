@import stdlib

func rule110.match000 () (int) in 0 end
func rule110.match001 () (int) in 1 end
func rule110.match010 () (int) in 1 end
func rule110.match011 () (int) in 1 end
func rule110.match100 () (int) in 0 end
func rule110.match101 () (int) in 1 end
func rule110.match110 () (int) in 1 end
func rule110.match111 () (int) in 0 end

func rule110.match_00 (int) (int) in
    0 = if rule110.match000 else rule110.match100 fi
end

func rule110.match_01 (int) (int) in
    0 = if rule110.match001 else rule110.match101 fi
end

func rule110.match_10 (int) (int) in
    0 = if rule110.match010 else rule110.match110 fi
end

func rule110.match_11 (int) (int) in
    0 = if rule110.match011 else rule110.match111 fi
end

func rule110.match__0 (int, int) (int) in
    0 = if rule110.match_00 else rule110.match_10 fi
end

func rule110.match__1 (int, int) (int) in
    0 = if rule110.match_01 else rule110.match_11 fi
end

func rule110.match (int, int, int) (int) in
    0 = if rule110.match__0 else rule110.match__1 fi
end

func rule110.xyz (ptr, range) (int, int, int) in
    dup2 range.wrap_dec -- xs, r, xs, i-1
    swp ptr.int + int.ptr ptr.@byte -- xs, r, x
    rot' dup2 range.i -- (x, xs, r, xs, i)
    swp ptr.int + int.ptr ptr.@byte -- x, xs, r, y
    rot' range.wrap_inc -- x, y, xs, i+1
    swp ptr.int + int.ptr ptr.@byte -- x, y, z
end

func rule110' (ptr, ptr, range) () in -- xs', xs, r
    dup rot' -- xs', r, xs, r
    rule110.xyz -- xs', r, x, y, z
    rule110.match -- xs', r, v
    swp range.i -- xs', v, i
    rot ptr.int + int.ptr -- v, xs'+i
    swp ptr.!byte -- ()
end

func rule110.rec (ptr, ptr, range) () in -- xs', xs, r
    dup range.< if -- xs', xs, r
        dup3 rule110' range.step rule110.rec
    else
        pop pop pop
    fi -- ()
end

func rule110 (ptr, int) (ptr) in -- xs, n
    dup ptr.alloc -- xs, n, xs'
    dup rot4' rot' -- xs', xs', xs, n
    0 swp range.init -- xs', xs', xs, r
    rule110.rec -- xs'
end

func ptr.out (ptr, int, int) () in -- xs, i, n
    dup2 < if -- xs, i, n
        rot' dup2 swp ptr.int + int.ptr -- n, xs, i, xs+i
        ptr.@byte int.show string.stdout -- n, xs, i
        1 + rot ptr.out
    else
        "\n" string.stdout pop pop pop
    fi -- ()
end

data range (int i, int j)

func range.init (int, int) (range) in
    16 ptr.alloc -- i, j, ptr
    dup ptr.int 8 + int.ptr rot ptr.!int -- i, ptr
    dup                     rot ptr.!int -- ptr
    ptr.range -- range
end

func range.i (range) (int) in
    range.ptr ptr.int 0 + int.ptr ptr.@int
end

func range.j (range) (int) in
    range.ptr ptr.int 8 + int.ptr ptr.@int
end

func range.< (range) (bool) in
    dup range.i swp range.j <
end

func range.step (range) (range) in
    dup range.i 1 + swp range.j range.init
end


func range.wrap_dec (range) (int) in
    dup range.i 0 = if range.j else range.i fi 1 -
end

func range.wrap_inc (range) (int) in
    dup dup range.i swp range.j 1 - = if pop 0 else range.i 1 + fi
end

func main () (int) in
    14 ptr.alloc -- (ptr)

    dup ptr.int 0  + int.ptr 0 ptr.!byte
    dup ptr.int 1  + int.ptr 0 ptr.!byte
    dup ptr.int 2  + int.ptr 0 ptr.!byte
    dup ptr.int 3  + int.ptr 1 ptr.!byte
    dup ptr.int 4  + int.ptr 0 ptr.!byte
    dup ptr.int 5  + int.ptr 0 ptr.!byte
    dup ptr.int 6  + int.ptr 1 ptr.!byte
    dup ptr.int 7  + int.ptr 1 ptr.!byte
    dup ptr.int 8  + int.ptr 0 ptr.!byte
    dup ptr.int 9  + int.ptr 1 ptr.!byte
    dup ptr.int 10 + int.ptr 1 ptr.!byte
    dup ptr.int 11 + int.ptr 1 ptr.!byte
    dup ptr.int 12 + int.ptr 1 ptr.!byte
    dup ptr.int 13 + int.ptr 1 ptr.!byte

    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    dup 0 14 ptr.out -- ptr
    14 rule110 -- ptr
    pop

    0
end
