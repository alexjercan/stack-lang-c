func dup2 (a, b) (a, b, a, b) in
    swp dup rot dup rot'
end

func pop2 (a, b) () in
    pop pop
end

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

func show' (int) (string) in
    dup 0 = if pop ""
    else dup 10 mod swp 10 / show' swp digit.show string.concat fi
end

func show (int) (string) in
    dup 0 = if pop "0"
    else show' fi
end

func <= (int, int) (bool) in
    dup2 < if pop2 true else = if true else false fi fi
end

func and (bool, bool) (bool) in
    if if true else false fi else false fi
end
