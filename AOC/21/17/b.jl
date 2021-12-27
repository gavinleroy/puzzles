# Gavin Gray AOC 21

struct Area
    x1
    x2
    y1
    y2 :: Int
end

simulate(v0, a ::Area) = Channel{Vector{Int}}() do c
    v = [0, 0]
    while(v[1] <= a.x2 && v[2] >= a.y1)
        v = v + v0
        v0 -= [1, 1]
        v0[1] = v0[1] < 0 ? 0 : v0[1]
        put!(c, v)
    end
end

function hit(xv, yv, a ::Area)
    for pos in simulate([xv, yv], a)
        if a.x1 <= pos[1] <= a.x2 && a.y1 <= pos[2] <= a.y2
            return true
        end
    end
    return false
end

function solve(a ::Area)
    ans = 0
    for xv in 0:a.x2
        for yv in a.y1:(-a.y1 - 1)
            if hit(xv, yv, a)
                ans += 1
            end
        end
    end
    ans
end

# Main
R = r"target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)"
l = readline()
matches = map(s -> parse(Int, s), match(R, l))
println(solve(Area(matches...)))
