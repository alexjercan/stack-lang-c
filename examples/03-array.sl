@import stdlib

func show_items' (int, array) () in -- i, a
    dup array.count -- i, a, c
    rot dup rot -- a, i, i, c
    >= if -- a, i
        pop2
    else
        dup2 -- a, i, a, i
        array.get -- a, i, ptr, bool
        unwrap -- a, i, ptr
        int.* int.show "\n" string.concat string.stdout -- a, i
        1 + swp -- i+1, a
        show_items' -- ()
    fi -- ()
end

func show_items (array) () in 0 swp show_items' end

func main () (int) in
    int.sizeof array.init.with_sz -- a
    dup 69 int.& array.append unwrap -- a
    dup 42 int.& array.append unwrap -- a

    int.sizeof array.init.with_sz -- a, a'
    dup 1 int.& array.append unwrap -- a, a'
    dup 2 int.& array.append unwrap -- a, a'
    dup 3 int.& array.append unwrap -- a, a'
    dup 4 int.& array.append unwrap -- a, a'

    dup 1 array.delete unwrap -- a, a'

    dup2 array.extend unwrap pop -- a

    dup 1 array.delete unwrap -- a

    int.sizeof array.init.with_sz -- a, a'
    dup 33 int.& array.append unwrap -- a, a'

    dup2 1 swp array.insert_many unwrap pop -- a

    show_items 0
end
