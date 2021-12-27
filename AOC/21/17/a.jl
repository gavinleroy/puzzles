# Gavin Gray AOC 21

struct Area
    x1
    x2
    y1
    y2 :: Int
end

function solve(a ::Area)
    (a.y1 * (a.y1 + 1)) / 2
end

# Main
R = r"target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)"
l = readline()
matches = map(s -> parse(Int, s), match(R, l))
println(solve(Area(matches...)))
