function y = roundp(number, precision)
## -*- texinfo -*-
## @deftypefn roundp(@var{number}, @var{precision})
## Receives a number and an integer for the precision to round to, where precision = 10^p.
## @end deftypefn
    if (nargin != 2)
        error("Requires a number and a precision of 10^p to round the number to");
    end

    if (!isnumeric(number))
        error("The first parameter must be a numeric value");
    elseif (!isinteger(precision) && !isscalar(precision))
        error("The second parameter must be an integer");
    end

    p = 10^precision;
    y = round(number*p)/p;
end
