#!/usr/bin/env python3

#given the positive integers a and b, find the largest common factor

a = 360
b = 756

c = 0

while True:
    if (a < b):
        a,b = b,a

    if (a == b):
        print("gcd:", a)
        exit(0)

    c = a - b
    if (c >= b):
        a = c
    else:
        a = b
        b = c

