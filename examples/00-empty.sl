@import stdlib

data cli_args
    ( int offset
    )

func cli_args.init.empty () (cli_args) in
    0 cli_args.init
end

func cli_args.usage () () in
end

func cli_args.parse' (array, int, cli_args) (cli_args) in -- array, i, args
    rot' -- cli_args, array, int
    dup2 swp array.count < if -- args, array, i
        dup2 array.get unwrap -- args, array, i, ptr
        ptr.* string.init.cstr -- args, array, i, string

        dup "--help" string.= swp dup "-h" string.= rot or if -- args, array, i, string
            pop cli_args.usage 0 sys.exit
        else
            rot4 -- array, i, string, args
            dup rot -- array, i, args, args, string
            int.read unwrap -- array, i, args, args, int
            cli_args.offset.set -- array, i, args
            rot' -- args, array, i
        fi -- args, array, i

        1 + rot cli_args.parse' -- args
    else
        pop2
    fi
end

func cli_args.parse (int, ptr) (cli_args) in -- argc, argv
    swp dup ptr.sizeof rot4 array.init 1 cli_args.init.empty cli_args.parse'
end

func main () (int) in
    "STACK_HOME" os.env.get unwrap "Stack home is: `" swp string.concat "`\n" string.concat string.stdout
    0
end
