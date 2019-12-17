using Primes

function main()
	p = parse(Int, readline())
	for _ in 1:p
		println(((isprime(parse(Int, readline()))) ? "Prime" : "Not Prime"))
	end
end

main()
