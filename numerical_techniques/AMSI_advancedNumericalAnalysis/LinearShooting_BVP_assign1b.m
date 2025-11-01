clear all;

% this is a linear shooting method to compute 
% the solution of y'' =  p(x) y' + q(x) y + r(x), 
% y(a) = alpha and y(b) = beta. xspan gives 
% the x-values where we want to compute the 
% solution.

% Inputs
a=0; b=pi/2;
alpha=-0.3; beta=-0.1;
rhs1 = @(x,y) [y(2);
               y(2) + 2*y(1) + cos(x)];
rhs2 = @(x,u) [u(2);
               u(2) + 2*u(1)];
N=100; % resolution

xspan =linspace(a,b,N);
y0=[alpha;0];
[x, y] = rk4fun(rhs1, a,b,y0,N);
u0=[0;1];
[x, u] = rk4fun(rhs2, a,b,u0,N);
sol = y +(beta-y(end,1))/(u(end,1))*u;
yexact = @(x) -(1/10)*(sin(x) + 3*cos(x));
ye = yexact(x);

%subplot(1,2,1);
plot(x, sol(:,1),'linewidth',2);
%set(gca,'fontsize',15);
title("Numerical solution");


%subplot(1,2,2);
plot(x,abs(ye-sol(:,1)),'linewidth',2);
%set(gca,'fontsize',15);
title("Errors");
disp(["maxErr", max(abs(ye-sol(:,1)))]);