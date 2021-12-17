# Gavin Gray AOC 21

struct Area
    x1
    x2
    y1
    y2 :: Int
end

function any(f, it)
    my = typemin(Int64)
    found = false
    for v in it
        my = max(my, v[2])
        found = found || f(v)
    end
    found, my
end

simulate(x, y) = Channel{Tuple{Int, Int}}() do c
    ix, iy = 0, 0
    while(true)
        ix += x; iy += y
        x = x > 0 ? x-1 : 0
        y -= 1; put!(c, (ix, iy))
    end
end

function solve(a ::Area)
    pred = posn -> posn[1] <= a.x2 && posn[2] >= a.y1
    inarea = posn -> a.x1 <= posn[1] && posn[1] <= a.x2 && posn[2] >= a.y1 && posn[2] <= a.y2
    maximum(map(function(p)
        line = Iterators.takewhile(pred, simulate(p...))
        res = any(inarea, line)
        res[1] ? res[2] : typemin(Int64)
      end, Iterators.product(0:200, 0:200)))
end

# Main
R = r"target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)"
l = readline()
matches = map(s -> parse(Int, s), match(R, l))
println(solve(Area(matches...)))
