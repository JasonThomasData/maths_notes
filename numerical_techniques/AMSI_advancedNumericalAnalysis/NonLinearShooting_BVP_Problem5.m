%this is a nonlinear shooting method to compute 
%the solution of y'' =  f(x,y,y')
% y(a) = alpha and y(b) = beta. xspan gives
% the x-values where we want to compute the 
% solution.

% Inputs
a=1; b=3;
alpha=17; beta=43/3;
rhsn = @(x,y) [y(2);
               4 + (1/4)*x.^3 - (1/8)*y(1)*y(2);
               y(4);
               -(1/8)*y(2)*y(3) - (1/8)*y(1)*y(4)];
options = odeset('RelTol',1e-12,'AbsTol',1e-12);
tol = 1e-12;
n = 20;

xspan=linspace(a,b,n);
tk=0;
y=zeros(n,4);
while (abs(y(end,1) - beta) > tol)
    y0=[alpha;tk;0;1];
    %[x, y] = ode45 (rhsn, xspan, y0,options);
    %[x, y] = ode45 (rhsn, xspan, y0);
    [x,y] = rk4fun(rhsn,a,b,y0,n);
    tk= tk - (y(end,1)-beta)/(y(end,3));
end
figure;
plot(xspan,y(:,1),'linewidth',2);
set(gca,'fontsize',15);

yexact = @(x) x.^2 + 16./x;
ye = yexact(x);
figure; 
plot(xspan,abs(y(:,1)-ye),'linewidth',2);
set(gca,'fontsize',15);

disp(["maxErr", max(abs(y(:,1)-ye))]);
