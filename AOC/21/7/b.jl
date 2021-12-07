# Gavin Gray AOC 21

function burn(from, to)
    nth = n -> trunc(Int, (n ^ 2 + n) / 2)
    return nth(abs(to - from))
end

# I'm positive there's a faster way to do this
function main()
    vec = map(s -> parse(Int, s), split(readline(), ","))
    mn, mx = minimum(vec), maximum(vec)
    calc = m -> foldl((acc, v) -> acc + burn(m, v), vec; init=0)
    return minimum(map(calc, mn:mx))
end

println(main())
