#!/usr/bin/env python3

import random

coordinates = []

for i in range(20):
    coordinates.append((i,random.randrange(-3,3)))

print(coordinates)