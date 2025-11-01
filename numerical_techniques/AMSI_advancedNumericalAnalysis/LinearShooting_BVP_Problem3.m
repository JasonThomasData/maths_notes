clear all;

% this is a linear shooting method to compute 
% the solution of y'' =  p(x) y' + q(x) y + r(x), 
% y(a) = alpha and y(b) = beta. xspan gives 
% the x-values where we want to compute the 
% solution.

% Inputs
a=1; b=2;
alpha = 1; beta=2;
rhs1 = @(x,y) [y(2);
               -(2./x).*y(2) + (2./x.^2).*y(1) + sin(log(x))./x.^2];
rhs2 = @(x,u) [u(2);
               -(2./x).*u(2) + (2./x.^2).*u(1)];
N=20; % resolution

xspan =linspace(a,b,N);
y0=[alpha;0];
[x, y] = rk4fun(rhs1, a,b,y0,N);
u0=[0;1];
[x, u] = rk4fun(rhs2, a,b,u0,N);
sol = y +(beta-y(end,1))/(u(end,1))*u;
yexact = @(x) 1.1392070132*x - 0.03920701320./(x.^2) - 0.3*sin(log(x)) - 0.1*cos(log(x));
ye = yexact(x);

subplot(1,2,1);
plot(x, sol(:,1),'linewidth',2);
set(gca,'fontsize',15);
subplot(1,2,2);
plot(x,abs(ye-sol(:,1)),'linewidth',2);
set(gca,'fontsize',15);
disp(["maxErr", max(abs(ye-sol(:,1)))]);
