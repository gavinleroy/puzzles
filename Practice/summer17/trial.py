#
#############Day ##############
#
#
#from hashlib import md5
#from itertools import compress
#
#input = "ioramepc"
#moves = {
#    'U': lambda x, y: (x, y - 1),
#    'D': lambda x, y: (x, y + 1),
#    'L': lambda x, y: (x - 1, y),
#    'R': lambda x, y: (x + 1, y)
#}
#
#def doors(path):
#    string = (input + ''.join(path)).encode('utf-8')
#    return (int(x, 16) > 10 for x in md5(string).hexdigest()[:4])
#
#def bfs(start, goal):
#    queue = [(start, [start], [])]
#    while queue:
#        (x, y), path, dirs = queue.pop(0)
#        for dir in compress('UDLR', doors(dirs)):
#            next = moves[dir](x, y)
#            nx, ny = next
#            if not (0 <= nx < 4 and 0 <= ny < 4):
#                continue
#            elif next == goal:
#                yield dirs + [dir]
#            else:
#                queue.append((next, path + [next], dirs + [dir]))

#def day17():
#    paths = list(bfs((0, 0), (3, 3)))
#    return ''.join(paths[0]), len(paths[-1])
#
#print(day17())


###########Day 19##Part 2#######


#target = 3005290
#i = 1
#
#while i * 3 < target:
#    i *= 3
#
#print(target - i)

#########Day 1###############


#!/usr/bin/python3

#DIRECTIONS = 'NWSE'
#
#
#def read_data(fname):
#    data = []
#    for step in open("/users/gavingray/Desktop/input.txt").readline().split(','):
#        step = step.strip()
#        data.append((step[0], int(step[1:])))
#    return data
#
#
#def turn(direction_idx, direction):
#    if direction == 'R':
#        direction_idx += 1
#    else:
#        direction_idx -= 1
#    return direction_idx % len(DIRECTIONS)
#
#
#def distance(xy):
#    x, y = xy
#    return abs(x) + abs(y)
#
#
#def get_coordinates():
#    direction_idx = 0
#    x = 0
#    y = 0
#    data = read_data("input.txt")
#    
#    for rotate_direction, length in data:
#        direction_idx = turn(direction_idx, rotate_direction)
#        if DIRECTIONS[direction_idx] == 'N':
#            y -= length
#        elif DIRECTIONS[direction_idx] == 'S':
#            y += length
#        elif DIRECTIONS[direction_idx] == 'W':
#            x += length
#        elif DIRECTIONS[direction_idx] == 'E':
#            x -= length
#    return x, y
#
#
#def get_second_coordinates():
#    locations = set()
#    
#    x = 0
#    y = 0
#    direction_idx = 0
#    data = read_data("input.txt")
#    
#    for rotate_direction, length in data:
#        direction_idx = turn(direction_idx, rotate_direction)
#        if DIRECTIONS[direction_idx] == 'N':
#            for i in range(length):
#                if (x, y - i) in locations:
#                    return x, y - i
#                locations.add((x, y - i))
#            y -= length
#        elif DIRECTIONS[direction_idx] == 'S':
#            for i in range(length):
#                if (x, y + i) in locations:
#                    return x, y + i
#                locations.add((x, y + i))
#            y += length
#        elif DIRECTIONS[direction_idx] == 'W':
#            for i in range(length):
#                if (x + i, y) in locations:
#                    return x + i, y
#                locations.add((x + i, y))
#            x += length
#        elif DIRECTIONS[direction_idx] == 'E':
#            for i in range(length):
#                if (x - i, y) in locations:
#                    return x - i, y
#                locations.add((x - i, y))
#            x -= length
#
#print(distance(get_coordinates()))
#print(distance(get_second_coordinates()))




#def getInput():
#    with open('input.txt', 'r') as f:
#        return f.readlines()
#
#def solvePart1():
#    ipsBlocked = sorted([list(map(int, x.rstrip().split('-'))) for x in getInput()], key = lambda x:x[0])
#    lowestIP  = 0
#    for val in ipsBlocked:
#        if lowestIP >= val[0] and lowestIP <= val[1]:
#            lowestIP =  val[1]+1
#    return lowestIP
#
#def solvePart2():
#    ipsBlocked = sorted([list(map(int, x.rstrip().split('-'))) for x in getInput()], key = lambda x:x[0])
#    start = ipsBlocked[0][0]
#    n = ipsBlocked[0][1]
#    allowedIps = 0 + start
#    for i in range(1, len(ipsBlocked)):
#        if ipsBlocked[i][0] > n:
#            allowedIps += ipsBlocked[i][0] - n -1
#            n = ipsBlocked[i][1]
#        if ipsBlocked[i][1] > n:
#            n = ipsBlocked[i][1]
#    return allowedIps
#
#print("PART 1, The lowest-valued IP %d"%solvePart1())
#print("PART 2, Allowed IPs %d"%solvePart2())
























