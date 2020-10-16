#!/usr/bin/env Rscript

# When there are more than two variables for a linear programming problem, it becomes harder to use a graphical method.
# Eg a problem with four variables would require drawing on a four dimensional graph... which doesn't make sense
# For this, we can use the lPsolver package in R

#install.packages("lpSolveAPI")
library("lpSolveAPI")

objective_coefficients = c(25,10,5,21,6,1,25,10,5)

lp_model = make.lp(0,9) #intial constraints, initial decision variables
lp.control(lp_model, sense="max") #maximise for objective function
set.objfn(lp_model, objective_coefficients)
add.constraint(lp_model, c(1,1,1,0,0,0,0,0,0), "=", 3800)
add.constraint(lp_model, c(0,0,0,1,1,1,0,0,0), "=", 3200)
add.constraint(lp_model, c(0,0,0,0,0,0,1,1,1), "=", 3500)
add.constraint(lp_model, c( 1-0.55, -0.55, -0.55, 0,0,0,0,0,0), ">=", 0)
add.constraint(lp_model, c( -0.3, 1-0.3, -0.3, 0,0,0,0,0,0), ">=", 0)
add.constraint(lp_model, c(0,0,0, 1-0.45, -0.45, -0.45, 0,0,0), ">=", 0)
add.constraint(lp_model, c(0,0,0, -0.4, 1-0.4, -0.4, 0,0,0), ">=", 0)
add.constraint(lp_model, c(0,0,0,0,0,0, 1-0.3, -0.3, -0.3 ), ">=", 0)
add.constraint(lp_model, c(0,0,0,0,0,0, -0.5, 1-0.5, 0.5 ), ">=", 0)

solve(lp_model)
print("Optimum decision variables:")
print(get.variables(lp_model))
print("Optimum profit:")
print(sum(get.variables(lp_model) * objective_coefficients))
