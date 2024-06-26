h = 0.1;
upperBound = 1.2;

init = [1;0];

pkg load symbolic;
syms x  y;

u = [1;
    (2/x)*sqrt(y - log(x)) + 1/x];

u = [1];
init = [1];

yRK2 = RK2_Autonomous(u, init, h, upperBound);

xSpace = x0:h:upperBound;
yExact = exact(xSpace);

function res = ydash(y)
    res = (2/x)*sqrt(y - log(x)) + 1/x;
end

function res = exact(x)
    res = log(x).^2 + log(x);
end
