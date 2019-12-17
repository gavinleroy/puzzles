function main()
	n = parse(Int, readline())
	c = parse.(Int, split(readline(), ' '))
	i = 1
	ans = 0
	while i < n-2
		i += ((c[i+2] == 0) ? 2 : 1)
		ans += 1
	end
	ans += ((i < n) ? 1 : 0)
	return ans
end

println(main())
