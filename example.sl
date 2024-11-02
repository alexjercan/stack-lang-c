data ivec2 (int x, int y)

func ivec2.dot (ivec2, ivec2) (int) in
    dup ivec2.x swp ivec2.y rot
    dup ivec2.x swp ivec2.y rot
    * swp rot
    *
    +
end

data complicated (int n, ivec2 i)

func main () (int) in
    22 5 42 ivec2 complicated
    dup complicated.n swp
    complicated.i dup ivec2.y swp ivec2.x + +
end
