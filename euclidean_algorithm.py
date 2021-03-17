#!/usr/bin/env python3

#given the positive integers a and b, find the largest common factor

a = 6
b = 238

if (a < b):
    a,b = b,a

def shortcut(a,b):
    # Given gcd(a,b) -> a = bq + r -> gcd(b,r)
    # This essentially collapses the cyclical reduction of c = a - b to find the remainder with modulo
    r = a % b
    return b,r

a, b = shortcut(a, b)

c = 0
while True:
    if (a == b):
        print("gcd:", a)
        exit(0)

    c = a - b
    if (c >= b):
        a = c
    else:
        a = b
        b = c
