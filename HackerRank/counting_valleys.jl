function main()
	n = parse(Int, readline())
	aa = split(readline(), "")
	i = 0
	ans = 0
	for c in aa
		ans += ((i == 0 && c=="D") ? 1 : 0)
		i += ((c=="U") ? 1 : -1)
	end
	return ans
end

println(main())
