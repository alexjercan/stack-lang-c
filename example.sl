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

func out (int) () in
    -- TODO: implement out
    pop
end

func *** (int) (int) in
    dup out
    dup 10 <
    if 0 else 1 + *** fi
end

func main () (int) in
    -- 42 5 ivec2 22 complicated dup complicated.n swp complicated.i !!! +
    1 ***
end
