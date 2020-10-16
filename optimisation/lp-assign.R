#!/usr/bin/env Rscript

# Assignment example

# These simmers must be in a relay, and below are their times for different strokes.
# What is the optimal combination
m <- matrix(c(5,9,10,16,10,15,12,11,9,10,8,13,14,10,12,15), 4,
       dimnames = list(c("L", "S", "E", "P"), c("D", "C", "V", "L")))

library(lpSolve)
fm <- lp.assign(m)

print(m)
print(fm$solution)

# Next up is a separate task, a supply chain problem

# There are no deliveries between melbourne and brisbane
# Undelivered is required for dummy

cost_mat <- matrix(c(100,200,120,350,1000000000,220,150,300), 4,
       dimnames = list(c("Melbourne", "Perth", "Adelaide", "Undelivered"), c("Sydney", "Brisbane")))
print(cost_mat)

direction = "min"

# capacity may not be exceeded
row_signs <- rep("<=", 4)
row_rhs <- c(1000,1300,1200,200)

# demand must be satisfied
col_signs <- rep(">=",2)
col_rhs <- c(2300,1400)

solution <- lp.transport(cost.mat = cost_mat,
                         direction = direction,
                         row.signs = row_signs,
                         row.rhs = row_rhs,
                         col.signs = col_signs,
                         col.rhs = col_rhs)
                         
print(solution$solution)