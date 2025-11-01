clear all;

% this is a linear shooting method to compute 
% the solution of y'' =  p(x) y' + q(x) y + r(x), 
% y(a) = alpha and y(b) = beta. xspan gives 
% the x-values where we want to compute the 
% solution.

% Inputs
a=0; b=1;
alpha=0; beta=2;
rhs1 = @(x,y) [y(2);
               4*y(1) - 4*x];
rhs2 = @(x,u) [u(2);
               4*u(1)];
n=20; % resolution

xspan =linspace(a,b,n);
y0=[alpha;0];
[x, y] = rk4fun(rhs1, a,b,y0,n);
u0=[0;1];
[x, u] = rk4fun(rhs2, a,b,u0,n);

% Error calculation
sol = y +(beta-y(end,1))/(u(end,1))*u;
yexact = @(x) (exp(-2*x)-exp(2*x))/(exp(-2)-exp(2))+x;
ye = yexact(x);
errMax = max(abs(ye-sol(:,1)));
disp(["Max error:", errMax]);

subplot(1,2,1);
plot(x, sol(:,1),'linewidth',5);
set(gca,'fontsize',15);
subplot(1,2,2);
plot(x,abs(ye-sol(:,1)),'linewidth',5);
set(gca,'fontsize',15);
