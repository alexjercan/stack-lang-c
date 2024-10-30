data ivec2 (int x, int y)

func ivec2.dot ivec2 ivec2 -> int in
    dup ivec2.x swp ivec2.y rot
    dup ivec2.x swp ivec2.y rot
    * swp rot
    *
    +
end

func main in
    69
end
