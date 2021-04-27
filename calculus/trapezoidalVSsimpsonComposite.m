%#!/usr/bin/env octave


% The Trapezoidal Rule and Simpson Composite Rule are methods for approximating the area under a curve.
% This shows the error of each, when compared to the integral

clear;



intervalBounds = [1,2];
preciseArea = actualIntegral(intervalBounds(2)) - actualIntegral(intervalBounds(1));
disp(["The area, according to the definite integral, is", preciseArea]);
intervals = [10,100,200,400];

for ii =1:length(intervals)
    numberOfIntervals = intervals(ii);
    fprintf("For %d intervals\n", numberOfIntervals);
    approxAreaTrapezoidal = trapezoidalApproximation(@quadFunction, numberOfIntervals, intervalBounds);
    trapezoidalError = approxAreaTrapezoidal - preciseArea;
    fprintf("Error for trapezoidal is %f\n", trapezoidalError);
    approxAreaSimpson = simpsonCompositeApproximation(@quadFunction, numberOfIntervals, intervalBounds);
    simpsonError = approxAreaSimpson - preciseArea;
    fprintf("Error for simpson is %f\n", simpsonError);
end

function y = quadFunction(x)
    y = x^2 + 7*x + 4;
end

function y = actualIntegral(x) 
    y = (1/3)*x^3 + 3.5*x^2 + 4*x; % + C, but this won't matter when we find the area, C - C
end

function approxArea = simpsonCompositeApproximation(subjectFunction, numberOfIntervals, intervalBounds)
    intervalSize = (intervalBounds(2) - intervalBounds(1))/numberOfIntervals;
    disp(intervalSize)
    approxArea = 0;

    for ii =1:2:numberOfIntervals-1
        lowerBound = (ii-1)*intervalSize; % we want pairs of intervals, like [lower, middle, upper] = [[0,1,2],[2,3,4],[4,5,6] ... ]
        middle = (ii)*intervalSize;
        upperBound = (ii+1)*intervalSize;
        % (h/3)(f(x-1)+4*f(x)+f(x+1)) is the composite rule, iterating over x in (1,3,5,7,...)
        intervalPairArea = (intervalSize/3)*(subjectFunction(lowerBound)+(4*subjectFunction(middle))+subjectFunction(upperBound));
        approxArea = approxArea + intervalPairArea;
    end
end

function approxArea = trapezoidalApproximation(subjectFunction, numberOfIntervals, intervalBounds)
    intervalSize = (intervalBounds(2) - intervalBounds(1))/numberOfIntervals;
    approxArea = 0;

    for ii =1:numberOfIntervals
        lowerBound = (ii-1)*intervalSize;
        upperBound = ii*intervalSize;
        % (h)((f(x-1)+f(x))/2), is finding the area of a trapezoid by
        % finding the average of its sides and treating it as a rectangle
        intervalArea = intervalSize*((subjectFunction(lowerBound)+subjectFunction(upperBound))/2);
        approxArea = approxArea + intervalArea;
    end
end


