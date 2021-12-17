# Gavin Gray AOC 21

struct Area
    x1
    x2
    y1
    y2 :: Int
end

any(f, it) = foldl((x, y) -> x || y, map(f, it); init=false)

simulate(x, y) = Channel{Tuple{Int, Int}}() do c
    ix, iy = 0, 0
    while(true)
        ix += x; iy += y
        x = x > 0 ? x-1 : 0
        y -= 1; put!(c, (ix, iy))
    end
end

# FIXME I'll come back to this after the semester
#       and make it faster :)
function solve(a ::Area)
    pred = posn -> posn[1] <= a.x2 && posn[2] >= a.y1
    inarea = posn -> a.x1 <= posn[1] && posn[1] <= a.x2 && posn[2] >= a.y1 && posn[2] <= a.y2
    count(map(function(p)
        line = Iterators.takewhile(pred, simulate(p...))
        any(inarea, line)
      end, Iterators.product(0:300, -300:300)))
end

# Main
R = r"target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)"
l = readline()
matches = map(s -> parse(Int, s), match(R, l))
println(solve(Area(matches...)))
