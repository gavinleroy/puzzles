function main()
	s = readline()
	n = parse(Int, readline())
	c = 0
	for ch in s
		c += ((ch=='a') ? 1 : 0)
	end
	c *= div(n,length(s))
	ind = mod(n, length(s))
	while ind > 0
		c += ((s[ind] == 'a') ? 1 : 0)
		ind -= 1
	end
	return c
end

println(main())
