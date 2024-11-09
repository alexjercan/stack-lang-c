@import stdlib

data test (int x)

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

func main () (int) in
    100 fizzbuzz
    0
end
