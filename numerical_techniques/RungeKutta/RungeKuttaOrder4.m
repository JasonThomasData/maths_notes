function [x, y] = RungeKuttaOrder4(f,x0,y0,h,b)
    % This function solves numerically the initial value problem
    % for an ordinary differential equation of first order:
    %
    % y’ = f(x,y), y(x0) = y0,
    %
    % using Runge-Kutta method of order four
    % with mesh size h on the interval [x0,b].
    %
    % Input: f, a function of two variables,
    % x0, a number,
    % y0, another number,
    % h, a small number, step size of the mesh,
    % b, ‘‘right’’ end of the domain of the solution.
    % In fact, b can be < x0, in which case the solution is
    % prolonged to the left of the point x0.
    %
    % Output: y, vector containing values of the approximate
    % solution at mesh points;
    % x, a vector of mesh points.
    % Calculate the number of mesh intervals
    
    N = abs(b-x0)/h;
    % Create an array of mesh points
    if b>x0
        x=x0+h*(0:N);
    else
        x=x0+h*(0:-1:-N);
    end
    
    y=zeros(1,N+1);
    y(1) = y0; % initial value
    
    for k=1:N
        g1 = h*f(x(k),y(k));
        g2 = h*f(x(k)+h/2, y(k)+g1/2);
        g3 = h*f(x(k)+h/2, y(k)+g2/2);
        g4 = h*f(x(k+1), y(k)+g3);
        y(k+1) = y(k) + (g1+2*g2+2*g3+g4)/6;
    end
end