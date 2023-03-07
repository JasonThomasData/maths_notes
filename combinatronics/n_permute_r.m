function cardinality = n_permute_r(n, r)
    # order of choices matter
    # n is elements in the sample
    # r is the number of choices
    cardinality = factorial(n)/factorial(n-r);
end

