#!/usr/bin/env octave

pkg load symbolic

rootCount = 3;
rootRange = [-10,10];

syms x
root1 = randi(rootRange);
root2 = randi(rootRange);
root3 = randi(rootRange);
P = (x-root1)*(x-root2)*(x-root3);
disp(expand(P));