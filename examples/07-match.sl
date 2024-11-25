@import stdlib

func function (int, string, bool, array) (string, bool) in
    -- match will consume the items from the stack
    -- but make them available under the given names
    -- the names from the match statement cannot be redefined

    match ok xs in -- int, string
        string.& -- int, ptr
        xs -- int, ptr, array
        swp -- int, array, ptr
        array.append -- int
        xs -- int, array
        swp -- array, int
        array.get -- ptr
        string.* -- string
        ok -- string, bool
    end
end

func main () (int) in
    0
end
