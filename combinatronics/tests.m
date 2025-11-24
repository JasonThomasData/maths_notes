assert(nCr(10,1) == 10)
assert(nCr(10,5) == 252)
assert(nCr(10,10) == 1)

assert(nCRr(10,1) == 10)
assert(nCRr(10,5) == 2002)
assert(nCRr(10,10) == 92378)

assert(nPr(10,1) == 10)
assert(nPr(10,5) == 30240)
assert(nPr(10,10) == 3628800)

assert(nPRr(10,1) == 10)
assert(nPRr(10,5) == 100000)
assert(nPRr(10,10) == 10000000000)

disp("Passed")