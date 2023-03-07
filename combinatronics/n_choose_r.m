function cardinality = n_choose_r(n, r)
    # order of choices don't matter
    # n is elements in the sample
    # r is the number of choices
    cardinality = factorial(n)/(factorial(r)*factorial(n-r));
end

