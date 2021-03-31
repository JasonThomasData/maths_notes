#!/usr/bin/env octave

%find the root of a function given an interval for its domain.
%This is useful for times when you can't find the derivative.

% We are trying to approximate this where this intercepts the x axis, which
% is approx 1.34, visually. This is the root for the function within an
% interval

function xaxisIntercept = secant(precision, interval, f)
    format long;

    y = f(interval);
    plot(interval,y);

    x = [interval(1), interval(2)];
    secondLast = x(length(x)-1);
    last = x(length(x));

    halt = false;

    while ~halt
        next = last - f(last)*(last-secondLast)/(f(last)-f(secondLast));
        secondLast = last;
        last = next;
        x(end + 1) = next;
        if abs(last - secondLast) < precision
           halt = true;
        end
    end

    plot(1:length(x),x);
    title(sprintf("Precision approaching %d after %d iterations", x(length(x)), length(x))); 

    xaxisIntercept = last;
end

function y = findRootOf(x)
    y = cos(x) - log(x);
end

#secant(10^-8, [3,2], @findRootOf);
