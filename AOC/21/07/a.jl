# Gavin Gray AOC 21

import Statistics.median

function main()
    vec = map(s -> parse(Int, s), split(readline(), ","))
    m = trunc(Int, median(vec))
    ans = foldl((acc, v) -> acc + abs(m - v), vec; init=0)
    return ans
end

println(main())
