function findv!(aa, v1)
	(haskey(aa, v1) && aa[v1] != v1) && return aa[v1] = findv!(aa, aa[v1]) 
	return v1
end

function joinv!(aa, bb, t1, t2)
	v1 = findv!(aa, t1)
	v2 = findv!(aa, t2)
	if(v1 != v2)
		aa[v1] = aa[v2] = (v1 < v2) ? v1 : v2 
		w1 = (haskey(bb, v1)) ? bb[v1] : 1 
		w2 = (haskey(bb, v2)) ? bb[v2] : 1 
		bb[v1] = bb[v2] = (w1+w2)
	end
	return bb[v1]
end

function main()
	a = Dict()
	b = Dict()
	n = parse(Int, readline())
	lrg = 1
	for _ = 1:n
		q = parse.(Int, split(readline(), ' '))
		t = joinv!(a, b, q[1], q[2])
		lrg = ((t > lrg) ? t : lrg)
		println(lrg)
	end
end

main()
