@import stdlib

func args.dump (int, ptr, int) () in -- argc, argv, i
    rot dup2 >= if -- argv i argc
        pop3
    else
        -- i argc argv string
        rot dup ptr.* dup ptr.strlen swp string.init -- i, argc, argv string
        "\n" string.concat string.stdout -- i, argc, argv
        ptr.sizeof ptr.+ -- i, argc, argv+sz
        rot 1 + -- argc, argv, i+1
        args.dump -- ()
    fi -- ()
end

func main (int, ptr) (int) in -- argc, argv
    -- argc, argv
    0 args.dump 0
end
