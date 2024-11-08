@import stdlib

data ivec2 (int x, int y)

func ivec2.dot (ivec2, ivec2) (int) in
    dup ivec2.x swp ivec2.y rot
    dup ivec2.x swp ivec2.y rot
    * swp rot
    *
    +
end

func fizzbuzz& (int) () in
    dup 15 mod 0 = if pop "fizzbuzz\n" string.out else
    dup 5 mod 0 = if pop "buzz\n" string.out else
    dup 3 mod 0 = if pop "fizz\n" string.out else
    show "\n" string.concat string.out
    fi fi fi
end

func fizzbuzz' (int, int) () in
    dup2 <= if swp dup fizzbuzz& 1 + swp fizzbuzz' else pop2 fi
end

func fizzbuzz (int) () in
    1 swp fizzbuzz'
end

data node (int d, bool isnull, node next)

func node.null () (node) in
    0 true 0 node
end

func node.cons (int, node) (node) in
    false swp node
end

func node.show (node) (string) in
    dup node.isnull if pop "" else
        dup node.d show
        swp dup node.next node.isnull if swp else swp ", " string.concat fi
        swp node.next node.show string.concat
    fi
end

func main () (int) in
    node.null 0 swp node.cons 1 swp node.cons 2 swp node.cons 3 swp node.cons
    node.show string.out "\n" string.out

    100 fizzbuzz

    0
end
