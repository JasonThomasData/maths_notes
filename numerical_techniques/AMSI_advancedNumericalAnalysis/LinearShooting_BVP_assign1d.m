%clear all;
%linear finite difference method (BVP)

%Q1b from notes

%Presets
P = @(x) 1 + 0.*x;
Q = @(x) 2 + 0.*x; 
R = @(x) cos(x);
a=0; b=pi/2;
alpha = -0.3; beta = -0.1; 
N = 100;

H = (b-a)/(N+1);
xf= linspace(a,b,N+2);
xi = xf(2:end-1);
A = 2+H^2*Q(xi);                   %diagonal
B = [0,-1+H/2*P(xi(1:end-1))];     %upper diagonal
C = [-1-H/2*P(xi(2:end)),0];       %lower diagonal
LM = spdiags([C',A',B'],-1:1,N,N); %matrix for the linear system

rhs = zeros(1,N);
rhs(1) = -H^2*R(xi(1))+(1+0.5*H*P(xi(1)))*alpha;
rhs(2:N-1) = -H^2*R(xi(2:end-1));
rhs(N) = -H^2*R(xi(end))+(1-0.5*H*P(xi(end)))*beta;

sol = LM\rhs';
sol =[alpha;sol;beta];

yexact = @(x) -(1/10).*(sin(x) + 3*cos(x));
ye = yexact(xf);

%plot(xf,ye,'linewidth',2);
%hold on;
plot(xf,sol,'linewidth',2);
legend('Numerical', 'exact');

errmax = max(abs(ye'-sol));
disp(["errmax", errmax])
