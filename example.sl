data ivec2 (int x, int y)

func ivec2.dot (ivec2, ivec2) (int) in
    dup ivec2.x swp ivec2.y rot
    dup ivec2.x swp ivec2.y rot
    * swp rot
    *
    +
end

data complicated (int n, ivec2 i)

func !!! (ivec2) (int) in
    dup ivec2.y swp ivec2.x +
end

func *** (int) (int) in
    dup 6 = if dup 48 + out fi
    dup 9 = if dup 48 + out fi
    dup 10 <
    if 1 + *** else 0 fi
end

func main () (int) in
    22 42 5 ivec2 complicated dup complicated.n swp complicated.i !!! + out
    10 out
    1 ***
end
