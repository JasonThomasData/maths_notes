#!/usr/bin/env octave

function y = f(x)
    y = sin(x) - log(x);
end

# Run this to see the actual dydx
function dydx(x)
    cos(x) - 1/x
end

# https://en.wikipedia.org/wiki/Finite_difference#Relation_with_derivatives
function approximation = finite_difference(x, k)
    h = 1/k;
    approximation = (f(x + h) - f(x))/h;
end

targetPrecission = 10^(-6);
halt = false;
limitDenominator = 1;
lastApproximation = 0;
x = 2;
while !halt
    approximation = finite_difference(x, limitDenominator);
    if(abs(approximation - lastApproximation) < targetPrecission)
        disp(approximation);
        halt = true;
    else
        lastApproximation = approximation;
        limitDenominator += 1;
    end
end