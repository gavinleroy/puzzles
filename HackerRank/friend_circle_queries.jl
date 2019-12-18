function findv!(aa, v1)
	aa[v1] != -1 && return aa[v1] = findv!(aa, aa[v1]) 
	return v1
end

function joinv!(aa, bb, t1, t2)
	v1 = findv!(aa, t1)
	v2 = findv!(aa, t2)
	if(v1 != v2)
		aa[(v1 < v2) ? v2 : v1] = (v1 < v2) ? v1 : v2 
		bb[v1] = bb[v2] = (bb[v1] + bb[v2])
	end
	return bb[v1]
end

function main()
	a = fill(-1, 10)
	b = fill(1, 10)
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
