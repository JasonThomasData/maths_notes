%clear all;
%linear finite difference method (BVP)

%Q1a from notes

%Presets
P = @(x) 0.*x;
Q = @(x) 4 + 0.*x; 
R = @(x) -4.*x;
alpha = 0; beta = 2; 
N = 20;
a = 0; b = 1;

H = (b-a)/(N+1);
xf= linspace(a,b,N+2);
xi = xf(2:end-1);
A = 2+H^2*Q(xi); %diagonal
B = [0,-1+H/2*P(xi(1:end-1))];%upper diagonal
C = [-1-H/2*P(xi(2:end)),0]; %lower diagonal
LM = spdiags([C',A',B'],-1:1,N,N); %matrix for the linear system

rhs = zeros(1,N);
rhs(1) = -H^2*R(xi(1))+(1+0.5*H*P(xi(1)))*alpha;
rhs(2:N-1) = -H^2*R(xi(2:end-1));
rhs(N) = -H^2*R(xi(end))+(1-0.5*H*P(xi(end)))*beta;

sol = LM\rhs';
sol =[alpha;sol;beta];

yexact = @(x) x+ (exp(2*x)-exp(-2*x))/(exp(2)-exp(-2));
ye = yexact(xf);
err = norm(ye'-sol);
errMax = max(abs(ye'-sol));
disp(["Max error:", errMax]);
plot(xf,ye);
hold on;
plot(xf,sol);
legend('exact','numerical', 'fontsize',16);
figure;
plot(xf,abs(ye-sol'));
legend('absolute error', 'fontsize',16);
