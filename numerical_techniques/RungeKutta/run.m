h = 0.1;
upperBound = 1.2;

format long;

<<<<<<< Updated upstream
pkg load symbolic;
syms x  y;
=======
%%
% 
%   for x = 1:10
%       disp(x)
%   end
% 
>>>>>>> Stashed changes

x0 = 1;
y0 = 0;

% Not autonomous
yRK1 = RK1(@ydash, x0, y0, h, upperBound);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now for the autonomous equations

init = [x0; y0];

syms x y;
f = [1;
     (2/x)*sqrt(y - log(x)) + 1/x];

RK1_a = RK1_auton(f, init, [x;y], h, upperBound);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Now for the autonomous equation RK4

N = round(abs(upperBound-init(1))/h)+1;
RK4_a = RK4_auton(f, init, [x;y], h, N);
[~, yRK4] = RungeKuttaOrder4(@ydash, x0, y0, h, upperBound);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compare to exact

xSpace = x0:h:upperBound;
yExact = exact(xSpace);

function res = ydash(x, y)
    res = (2/x)*sqrt(y - log(x)) + 1/x;
end

function res = exact(x)
    res = log(x).^2 + log(x);
end
