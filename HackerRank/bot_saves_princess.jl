function main()
	n = parse(Int, readline())
	m = [readline() for _ in 1:n]
	px, py, mx, my = 0,0,0,0
	for i in 1:n
		for j in 1:n
			if (m[i][j] == 'm') mx, my = i, j 
			elseif (m[i][j] == 'p') px, py = i, j end
		end
	end
	dx = mx-px; dy = my-py;
	ans = """"""
	if(dx>0) 
		ans *= "RIGHT\n"^dx 
	else 
		ans *= "LEFT\n"^abs(dx)  
	end
	if(dy>0)
		ans *= "DOWN\n"^dy
	else
		ans *= "UP\n"^abs(dy)
	end
	println(ans)	
end

main()
