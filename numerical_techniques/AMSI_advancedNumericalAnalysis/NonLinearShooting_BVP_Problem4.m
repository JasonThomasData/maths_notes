%this is a nonlinear shooting method to compute 
%the solution of y'' =  f(x,y,y')
% y(a) = alpha and y(b) = beta. xspan gives
% the x-values where we want to compute the 
% solution.

% Inputs
a=1; b=2;
alpha=0; beta=log(2);
rhsn = @(x,y) [y(2);
               -y(2)^2 - y(1) + log(x);
               y(4);
               -y(3) - 2*y(3)*y(4)];
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
plot(xspan,y(:,1),'linewidth',5);
set(gca,'fontsize',15);

yexact = @(x) log(x);
ye = yexact(x);
figure; 
plot(xspan,abs(y(:,1)-ye),'linewidth',5);
set(gca,'fontsize',15);

disp(["maxErr", max(abs(y(:,1)-ye))]);
