function main()
        n = parse(Int, readline())
        by, bx = parse.(Int, split(readline(), ' ')).+=1
        m = [readline() for _ = 1:n]
        px, py = 0, 0
        for i in 1:n
                for j in 1:n
                        if m[i][j] == 'p' py, px = i, j end
                end
        end
        dx = bx-px
        dy = by-py
        dy > 0 && return "UP"
        dy < 0 && return "DOWN"
        dx > 0 && return "LEFT"
        dx < 0 && return "RIGHT"
end

println(main())
