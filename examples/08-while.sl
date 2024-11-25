@import stdlib

func iterate (int, int, string) () in
    -- i, N, message
    rot' -- message, i, N

    -- the while loop will expect the same input and output
    -- the `while` keyword will represent the label where the
    -- program will jump after finishing a loop
    -- then is the condition which must generate a bool
    -- between loop and pool is defined the code that runs in a loop

    while -- message, i, N
        dup2 < loop

        ... stuff ...

    pool -- message, i, N
end

func iterate.better (int, int, string) () in
    -- using while loops with match statements should make managing
    -- conditions easier

    match i N message in -- ()
        i while -- i
            dup N < loop -- i
            message string.stdout -- i
            1 + -- i+1
        pool
    end
end

func main () (int) in
    0
end

