function main()
	b = parse(Int64, readline())
	s = readline();
	ar = parse.(Int64, split(s, ' '));
	dic = fill(0, 100)
	for i in ar
		dic[i] += 1;
	end
	n = 0;
	for v in dic
		n += v%2
	end
	return div((b - n),2)
end

println(main())
