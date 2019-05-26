#!/bin/python

t1, t2, n = raw_input().split()
t1 = int(t1)
t2 = int(t2)
n = int(n)

t3 = t1 + (t2**2)
for _ in range(n-3):
    t1 = t2
    t2 = t3
    t3 = t1 + (t2**2)
print t3
