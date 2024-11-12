data int extern
data bool extern
data string extern
data memory extern

func dup (a) (a, a) extern
func swp (a, b) (b, a) extern
func rot (a, b, c) (b, c, a) extern
func pop (a) () extern

func + (int, int) (int) extern
func - (int, int) (int) extern
func * (int, int) (int) extern
func / (int, int) (int) extern
func % (int, int) (int) extern
func > (int, int) (bool) extern
func < (int, int) (bool) extern
func = (int, int) (bool) extern
func string.out (string) () extern
func string.len (string) (int) extern
func string.concat (string, string) (string) extern
func string.substr (string, int, int) (string) extern
func memory.allocate (int) (memory) extern
-- func memory.@ (memory, int, a) (memory) extern
func memory.@ (memory, int, int) (memory) extern
-- func memory.!! (memory, int) (a) extern
func memory.!! (memory, int) (int) extern

func rot' (a, b, c) (c, a, b) in
    rot rot
end

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

func not (bool) (bool) in
    if false else true fi
end

func and (bool, bool) (bool) in
    if else pop false fi
end

func or (bool, bool) (bool) in
    if pop true else fi
end
