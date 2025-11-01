a=1; b=2;
alpha=1/2; beta=1/3;
rhsn = @(x,y) [y(2);
                                 y(1)^3 - y(1).*y(2);
                                 y(4);
                                (3*y(1)^2 - y(2))*y(3) - y(1)*y(4)];

tol = 1e-12;
n = 10+1;
xspan =linspace(a,b,n);
tk=0; y=zeros(n,4);
while (abs(y(end,1) - beta) > tol)
    y0=[alpha;tk;0;1];
    [x,y] = rk4fun(rhsn,a,b,y0,n);
    tk= tk - (y(end,1)-beta)/(y(end,3));
end
figure;
plot(xspan(1,2:(n-1)),y(2:(n-1),1),'linewidth',5);
set(gca,'fontsize',15);

disp(["maxErr", max(abs(y(2:(n-1),1)-ye(2:(n-1),1)))]);