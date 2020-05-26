#!/usr/bin/python

import math

ff = [math.factorial(x) for x in xrange(101)]

T = int(raw_input())

for _ in xrange(T):
    print ff[int(raw_input())]
