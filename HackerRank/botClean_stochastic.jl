function main()
	p = parse.(Int, split(readline(), ' ')).+=1
	m = [readline() for _ = 1:5]
	dd = [[y,x] for y in 1:5 for x in 1:5 if m[y][x] == 'd']
	dy, dx = p-dd[1]
	dy<0 && return "DOWN"
	dy>0 && return "UP"
	dx<0 && return "RIGHT"
	dx>0 && return "LEFT"
	return "CLEAN"
end

println(main())
