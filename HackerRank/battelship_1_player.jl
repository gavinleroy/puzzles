FILE = "MY_FILE.txt"
N = 9
euclid(x, y) = return sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2)
p(x) = return parse.(Int, split(x, ' '))
find_best_open(o) = r_w(o[rand(1:length(o))])
mi(m, p) = return (p[1]∈0:N && p[2]∈0:N) ? m[p[1]+1][p[2]+1]  : 'h'
r_w(p) = return "$(p[1]) $(p[2])"

function find_closest(h, o)
        aa=[]
        for t ∈ h
                aa = [i for i ∈ o if euclid(t, i)==1.0]
                !isempty(aa) && break
        end
        p = aa[1]
        return r_w(p)
end

function find_vector(m,h)
	for i ∈ h
		for j ∈ h
			if euclid(i, j)==1.0 
				mi(m,i - (j-i))=='-' && return r_w(i-(j-i))
				mi(m,j + (j-i))=='-' && return r_w(j+(j-i))
			end
		end
	end
	return false
end

function main()
        global N = parse(Int, readline())-1
        m = [readline() for _ ∈ 0:N]
	
        # Find all the hits
        h = [[y,x] for y ∈ 0:N for x ∈ 0:N if m[y+1][x+1]=='h'] 
	# Check if there are hits next to hits.
	r = find_vector(m,h)
	typeof(r) == String && return r
        # Find all open squares
        o = [[y,x] for y ∈ 0:N for x ∈ 0:N if m[y+1][x+1]=='-']
        isempty(h) && return find_best_open(o)
        return find_closest(h, o)
end

println(main())
