function cardinality = nPr(n, r)
    # order of choices matter
    # n is elements in the sample
    # r is the number of choices

    # The below code is correct but inefficient
    # cardinality = factorial(n)/factorial(n-r);
    cardinality = prod(n-r+1:1:n);
end

