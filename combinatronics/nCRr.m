function cardinality = nCRr(n, r)
    # order of choices don't matter
    # n is elements in the sample
    # r is the number of choices

    # The below code is correct but inefficient
    # cardinality = factorial(n+r-1)/(factorial(r)*factorial(n-1)))
    cardinality = prod(n:1:n+r-1)/factorial(r);
end
