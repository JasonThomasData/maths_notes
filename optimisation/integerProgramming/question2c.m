function x = question2c(CostL, Aeq, beq, A, b)
    ub = ones([1,24]);
    lb = zeros([1,24]);
    intVars = 1:24;
    
    x = intlinprog(CostL, intVars, A, b, Aeq, beq, lb, ub);
end
