function slope_field(varargin)
## -*- texinfo -*-
## @deftypefn slope_field(@var{DIFFERENTIAL_EQUATION}, @var{SPACE_LIMITS})
## @deftypefnx slope_field(@var{DIFFERENTIAL_EQUATION}, @var{SPACE_LIMITS}, @var{PARTICULAR_SOLUTION}, @var{INITIAL_CONDITION})
## Receives a first order differential equation and plots a slope field.
## Optionally receives a particular solution and initial condition to plot.
## Does not solve equations and does not find particular solutions according to specified initial conditions.
## @end deftypefn

    PARTICULAR_SOLUTION = NA;
    INITIAL_CONDITION = NA;

    if (nargin == 4)
        PARTICULAR_SOLUTION = varargin{3};
        INITIAL_CONDITION = varargin{4};
    elseif (nargin ~= 2)
        error("You need either two or four parameters.");
    end

    DIFFERENTIAL_EQUATION = varargin{1};
    if (~isa(DIFFERENTIAL_EQUATION, 'function_handle'))
        error("The first parameter, which must be a first-order differential equation, needs to be passed as a function handle");
    end

    SPACE_LIMITS = varargin{2};
    if ((size(SPACE_LIMITS) ~= 4) ||
        (~isa(SPACE_LIMITS(1), 'double')) ||
        (~isa(SPACE_LIMITS(2), 'double')) ||
        (~isa(SPACE_LIMITS(3), 'double')) ||
        (~isa(SPACE_LIMITS(4), 'double')))
        error("The second parameter must be a vector of four integers that define the limits of linear spaces to depict the slope field");
    end

    if (~isa(PARTICULAR_SOLUTION, 'function_handle') && ~isna(PARTICULAR_SOLUTION))
        error("The third parameter was specified but it is not a function handle.");
    end

    if ((~isna(INITIAL_CONDITION)) &&
        ((size(INITIAL_CONDITION) ~= 2) ||
         (~isa(INITIAL_CONDITION(1), 'double')) ||
         (~isa(INITIAL_CONDITION(2), 'double'))))
        error("The fourth parameter must be a vector in the dim 2 linear space");
    end

    x_space_min = SPACE_LIMITS(1);
    x_space_max = SPACE_LIMITS(2);
    y_space_min = SPACE_LIMITS(3);
    y_space_max = SPACE_LIMITS(4);

    tick = abs(x_space_max - x_space_min) / 15;

    [X_grid, Y_grid] = meshgrid(x_space_min:tick:x_space_max, y_space_min:tick:y_space_max);
    X_space = linspace(x_space_min, x_space_max);
    Y_space = linspace(y_space_min, y_space_max);
    differential_function_grid = DIFFERENTIAL_EQUATION(X_grid, Y_grid, 0); # These are the values of y' = dy/dx

    # Look up normal vectors - https://www.cfm.brown.edu/people/dobrush/am33/Matlab/ch2/slopefields.html
    normal = sqrt(1 + differential_function_grid.^2);
    U = 1./normal;
    V = differential_function_grid./normal;

    # The field
    quiver(X_grid, Y_grid, U, V, 0.5, "k");
    hold on;

    line([x_space_min x_space_max], [0 0], "linestyle", ":", "color", "k");
    line([0 0], [y_space_min y_space_max], "linestyle", ":", "color", "k");

    if (isa(PARTICULAR_SOLUTION, 'function_handle') && ~isna(INITIAL_CONDITION))
        delete (findobj ("tag", "legend"))
        evaluated_solution = PARTICULAR_SOLUTION(X_space, Y_space);
        # Listing the vis in this way allows for avoiding duplicates
        p1 = plot(X_space, evaluated_solution, "r");
        p2 = plot(INITIAL_CONDITION(1), INITIAL_CONDITION(2), "mo");
        legend([p1 p2], 'Particular Solution', 'Initial Condition');
    end

    axis([x_space_min x_space_max y_space_min y_space_max]);
    axis square;

end