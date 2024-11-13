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

func range.wrap_dec (range) (int) in
    -- (r)
    dup -- (r, r)
    range.i -- (r, i)
    0 = if -- (r)
        range.j -- (int)
    else -- (r)
        range.i -- (int)
    fi
    1 - -- (int)
end

func range.wrap_inc (range) (int) in
    -- (r)
    dup -- (r, r)
    dup -- (r, r, r)
    range.i -- (r, r, i)
    swp -- (r, i, r)
    range.j -- (r, i, n)
    1 - -- (r, i, n-1)
    = if -- (r)
        pop 0 -- (int)
    else -- (r)
        range.i 1 + -- (int)
    fi
end

func rule110.xyz (ptr, range) (int, int, int) in
    dup2 -- (m, r, m, r)
    range.wrap_dec -- (m, r, m, i-1)
    ptr.!! -- (m, r, x)
    rot' -- (x, m, r)
    dup2 -- (x, m, r, m, r)
    range.i -- (x, m, r, m, i)
    ptr.!! -- (x, m, r, y)
    rot' -- (x, y, m, r)
    range.wrap_inc -- (x, y, m, i+1)
    ptr.!! -- (x, y, z)
end

func rule110' (rule110.buffer, range) () in
    -- (b, r)
    dup2 -- (b, r, b, r)
    swp -- (b, r, r, b)
    rule110.buffer.m -- (b, r, r, m)
    swp -- (b, r, m, r)
    rule110.xyz -- (b, r, x, y, z)
    rule110.match -- (b, r, v)
    swp -- (b, v, r)
    range.i -- (b, v, i)
    rot -- (v, i, b)
    rule110.buffer.m' -- (v, i, m')
    rot' -- (m', v, i)
    swp -- (m', i, v)
    ptr.@ -- (m')
    pop -- ()
end

func rule110.rec (rule110.buffer, range) () in
    -- (b, r)
    dup2 -- (b, r, b, r)
    dup -- (b, r, b, r, r)
    range.< if  -- (b, r, b, r)
        rule110' -- (b, r)
        range.step -- (b, r)
        rule110.rec -- ()
    else  -- (b, r, b, r)
        pop2 pop2 -- ()
    fi
end

data rule110.buffer (ptr m, ptr m')

func rule110 (ptr, int) (ptr) in
    dup ptr.allocate -- (m, n, m')
    swp -- (m, m', n)
    0 swp range -- (m, m', r)
    rot' -- (r, m, m')
    rule110.buffer -- (r, b)
    dup rule110.buffer.m' -- (r, b, m')
    rot' -- (m', r, b)
    swp -- (m', b, r)
    rule110.rec -- (m')
end

func ptr.out (ptr, int, int) () in
    dup2 -- (xs, i, n, i, n)
    < if -- (xs, i, n)
        rot' -- (n, xs, i)
        dup2 -- (n, xs, i, xs, i)
        ptr.!! -- (n, xs, i, a)
        show string.out -- (n, xs, i)
        1 + -- (n, xs, i + 1)
        rot -- (xs, i + 1, n)
        ptr.out -- ()
    else -- (xs, i, n)
        "\n" string.out
        pop pop pop
    fi
end

data range (int i, int j)

func range.< (range) (bool) in
    dup range.i swp range.j <
end

func range.step (range) (range) in
    dup range.i 1 + swp range.j range
end

func main () (int) in
    14 ptr.allocate -- (ptr)

    0  0 ptr.@
    1  0 ptr.@
    2  0 ptr.@
    3  1 ptr.@
    4  0 ptr.@
    5  0 ptr.@
    6  1 ptr.@
    7  1 ptr.@
    8  0 ptr.@
    9  1 ptr.@
    10 1 ptr.@
    11 1 ptr.@
    12 1 ptr.@
    13 1 ptr.@
    dup 0 14 ptr.out -- (ptr)

    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)
    14 rule110 -- (ptr)
    dup 0 14 ptr.out -- (ptr)

    pop

    0
end
