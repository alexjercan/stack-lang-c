@import stdlib

func show_items' (int, array) () in -- i, a
    dup array.count -- i, a, c
    rot dup rot -- a, i, i, c
    >= if -- a, i
        pop2
    else
        dup2 -- a, i, a, i
        array.get -- a, i, ptr, bool
        not if panic fi -- a, i, ptr
        int.* int.show "\n" string.concat string.stdout -- a, i
        1 + swp -- i+1, a
        show_items' -- ()
    fi -- ()
end

func show_items (array) () in 0 swp show_items' end

func main () (int) in
    int.sizeof array.init.with_sz -- a
    dup 69 int.& array.append not if panic fi -- a
    dup 42 int.& array.append not if panic fi -- a

    show_items 0
end
