#!/bin/python

t = int(raw_input())
for _ in range(t):
    sp=raw_input()
    n = int(raw_input())
    ans = 0
    for _x in range(n):
        ans = ans + int(raw_input())
    if(ans % n == 0):
        print "YES"
    else:
        print "NO"
