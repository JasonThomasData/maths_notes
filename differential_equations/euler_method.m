function approximation = euler_method(DIFFERENTIAL_EQUATION, DOMAIN, INITIAL_Y)
## -*- texinfo -*-
## @deftypefn slope_field(@var{DIFFERENTIAL_EQUATION}, @var{DOMAIN}, @var{INITIAL_Y_GRADIENT})
## Receives a first order differential equation, a linear space of X and an initial gradient. Computes an approximation for a solution.
## Reducing the delta between x values will obtain a more accurate approximation.
## @end deftypefn

    if (nargin ~= 3)
        error("Review the function documentation");
    end

    if (~isa(DIFFERENTIAL_EQUATION, 'function_handle'))
        error("The first parameter, which must be a first-order differential equation, needs to be passed as a function handle");
    end

    if (size(DOMAIN)(1) ~= 1)
        error("The second parameter must be a row vector");
    end

    if (~isnumeric(INITIAL_Y))
        error("The initial Y muse be numeric. It is the first dependent variable value of the first independent element");
    end

    Y = INITIAL_Y;
    approximation = [Y];

    for i=1:(length(DOMAIN)-1)
        gradient = DIFFERENTIAL_EQUATION(DOMAIN(i), Y);
        delta_Y = gradient * (DOMAIN(i+1) - DOMAIN(i));
        next_Y = Y + delta_Y;
        Y = next_Y;
        approximation = [approximation next_Y];
    end

    #axis([x_space_min x_space_max y_space_min y_space_max]);
    #axis square;

end