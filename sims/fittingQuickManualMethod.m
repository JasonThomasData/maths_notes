#!/usr/bin/env octave

% Fitting functions to data

pkg load symbolic

syms x
root1 = randi(10);
root2 = randi(10);
root3 = randi(10);
P = (x-root1)*(x-root2)*(x-root3);
disp(expand(P));

function [xData, noisyYData] = getDataToFit()
    % for this example, this is unknown to us... we are given the noisyYData
    syms x;
    actualFunction = (0.4*(x-2.1)*(x-4.9)*(x-7.1)) + 12;
    disp(expand(actualFunction));

    % This is a linear system that represents the functions.
    % Octave/Matlab knows that the first coefficient applies to x^3, second to x^2...
    coefficients = [2/5, -141/25, 5999/250, -43059/2500];
    xData = 1:0.2:10;
    yData = polyval(coefficients, xData);

    noisyYData = zeros(1,length(yData));
    randomData = rand(1,length(yData));
    for ii =1:length(yData)
        noisyYData(ii) = yData(ii) + (randomData(ii)*4)-2;
    end
end

clear;

[xData, noisyYData] = getDataToFit();

% plot(xData, noisyYData);

% If you were given this chart and asked to fit a function to it, so that
% you might do some calculus etc, then how would you do that?

% This function appears to have a single maximum and a single mimimum. This
% is a cubic function. If we were to adjust the data to have the maximum
% number of x intercepts that you can get, what would that look like?

plot(xData, noisyYData);
hold on;
adjustmentEstimate = 12;
adjustedYData = noisyYData - adjustmentEstimate;
plot(xData, adjustedYData);
hold on;

% Now, the line appears to intercept x at 2,5,7
% We can construct a polynomial like this

% function = (x-2)(x-5)(x-7)
syms x;
disp(expand((x-2)*(x-5)*(x-7)));
coefficients_1 = [1,-14,59,-70];
approximatedYData_1 = polyval(coefficients_1, xData);
plot(xData, approximatedYData_1);
hold on;
disp("Error:");
disp(sum(abs(approximatedYData_1 - adjustedYData)));

% The shape here is consistent with the messy data, but, it varies too much
% We can multiply the function by half to see if it fits better

% function = 0.4(x-2)(x-5)(x-7)
syms x;
disp(expand(0.4*(x-2)*(x-5)*(x-7)));
coefficients_2 = [2/5,-28/5, 118/5,-28];
approximatedYData_2 = polyval(coefficients_2, xData);
plot(xData, approximatedYData_2);
hold on;
disp("Error:");
disp(sum(abs(approximatedYData_2 - adjustedYData)));

% Almost there, but we now need to readjust the function to be where the
% original was before the data was adjusted to pass through the x axis

% function = 0.4(x-2)(x-5)(x-7)-12
syms x;
disp(expand(0.4*(x-2)*(x-5)*(x-7)+adjustmentEstimate));
coefficients_3 = [2/5,-28/5, 118/5,-16];
approximatedYData_3 = polyval(coefficients_3, xData);
plot(xData, approximatedYData_3);
hold on;
disp("Error:");
disp(sum(abs(approximatedYData_3 - noisyYData)));

legend("1. Messy data", "2. adjusted -12", "3. First fitting", "4. Second fitting", "5. Final");

input("Press 'ENTER' key to exit.")