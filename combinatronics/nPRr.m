function cardinality = nPRr(n, r)
    # order of choices matter
    # n is elements in the sample
    # r is the number of choices

    # The below code is correct but inefficient
    # cardinality = factorial(n)/factorial(n-r);
    cardinality = n^r;
end

