FILE = "MY_FILE.txt"

euclidean(x,y) = return sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2)
f(s) = return parse.(Int, split(s, ' '))

function w(d, p)
	filter!(x->x≠p, d)
	open(FILE, "w") do io
		for p in d
			write(io, "$(p[1]) $(p[2])\n")
		end
	end
	return "CLEAN"
end

function find(p, aa)
	ld = 1000
	l = [0,0]
	for i in aa 
		nd = euclidean(p, i)
		if ld > nd
			ld = nd
			l = i
		end
	end
	return l
end

function main()
	p = parse.(Int, split(readline(), ' ')).+=1
	m = [readline() for _ = 1:5]
	dd = ((isfile(FILE)) ? f.(readlines(FILE)) : [[y,x] for y in 1:5 for x in 1:5 if m[y][x] == 'd']) 
	dy, dx = p-find(p, dd)
	dy<0 && return "DOWN"
	dy>0 && return "UP"
	dx<0 && return "RIGHT"
	dx>0 && return "LEFT"
	return w(dd, p)
end

println(main())
