FILE = "MY_FILE.txt"
e(x,y) = return sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2)
find(p, aa) = return minimum([(e(p, i), i) for i ∈ aa])[2]

function merge(x, y)  
	s = ""
	for (xi, yi) in zip(x, y)
		s *= (xi == '-') ? xi : (xi == 'd') ? xi : yi
	end
	return s
end

function w(d,s,p::Array{Int64,1}=[-1,-1])
	if p ≠ [-1, -1]  
		d[p[1]] = d[p[1]][1:p[2]-1] * '-' * d[p[1]][p[2]+1:end]
	end
        open(FILE, "w") do io
                for l in d
                        write(io, l*'\n')
                end
        end
        return s
end

function main()
	p = parse.(Int, split(readline(), ' ')) .+ 1
	t = [readline() for _ ∈ 1:5]
	o = (isfile(FILE)) ?  readlines(FILE) : copy(t)
	m = merge.(o, t)
	dd = [[y,x] for y ∈ 1:5 for x ∈ 1:5 if m[y][x] == 'd']
	oo = [[y,x] for y ∈ 1:5 for x ∈ 1:5 if m[y][x] == 'o']
	np = p - ((isempty(dd)) ? find(p, oo) : find(p, dd))
	np[1] < 0 && return w(m,"DOWN")
	np[2] > 0 && return w(m,"LEFT")
	np[1] > 0 && return w(m,"UP")
	np[2] < 0 && return w(m,"RIGHT")
	return w(m,"CLEAN", p)
end

println(main())
