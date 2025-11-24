function cardinality = nCr(n, r)
    # order of choices don't matter
    # n is elements in the sample
    # r is the number of choices
    
    # The below line is correct but inefficient
    # cardinality = factorial(n)/(factorial(r)*factorial(n-r));
    cardinality = prod(n-r+1:1:n)/factorial(r);
end
