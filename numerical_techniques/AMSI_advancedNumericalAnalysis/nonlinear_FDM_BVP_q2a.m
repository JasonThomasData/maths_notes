%nonlinear finite difference method

% Inputs
tol = 1e-10; %tolerance
NN = 1000; %maximum number of iterations
 
f = @(x,y,yp) 1/8*(32 + 2*x.^3 - y.*yp); %right hand side
fy = @(x,y,yp) -yp/8;                    %partial derivative of f with respect to y
fyp = @(x,y,yp) -y/8;                    %partial derivative of f with respect to yp

N = 19;   % numer of points in the interval, exercise says to make h=0.1
alpha = 17;  % boundary values
beta = 43/3;
a = 1;       % x_0, x_{n+1}
b = 3;

% Static code
h = (b-a)/(N+1);
z = linspace(alpha,beta,N+2)';
z = z(2:end-1); % z_i \approx y(x_i)

A = zeros(N,1); % Centre diagonal
B = zeros(N,1); % Upper diagonal
C = zeros(N,1); % Lower diagonal
D = zeros(N,1); % RHS vector
 
for j=1:NN 
     
    x = a+h;
    t = (z(2)-alpha)/(2*h);       % FDM approx of 1st derivative
    A(1) = -2-h^2*fy(x,z(1),t);   % first comp. of main diagonal
    B(1) = 1-h/2*(fyp(x,z(1),t)); % first comp. of upper diagonal
    D(1) = (2*z(1)-z(2)-alpha + h^2*f(x,z(1),t)); %first comp. of right hand side

    for i = 2:N-1   %computing at inner points 
        x = a+i*h;
        t = (z(i+1)-z(i-1))/(2*h); % FDM approx of 1st derivative
        A(i) = -2-h^2*fy(x,z(i),t);
        B(i) = 1-h/2*fyp(x,z(i),t);
        C(i) = 1+h/2*fyp(x,z(i),t);
        D(i) = (2*z(i)-z(i+1)-z(i-1)+h^2*f(x,z(i),t));
    end

    x = b-h;
    t = (beta-z(N-1))/(2*h);
    A(N) = -2-h^2*fy(x,z(N),t); %last comp. of the main diagonal
    C(N) = 1+h/2*fyp(x,z(N),t); %last comp. of the lower diagonal
    D(N) = (2*z(N)-z(N-1)-beta+h^2*f(x,z(N),t)); %last comp. of right hand side

    C = [C(2:end);0];              % only A needs to be full size,  
    B = [0;B(1:end-1)];
    M = spdiags([C,A,B],-1:1,N,N); %M is the Jacobian matrix

    %Newton iteration
    v = M\D;
    z = z+v;
    if (norm(v) <= tol)
        break;
    end
end

z = [alpha;z;beta];


% Error calculation
yexact = @(x) x.^2 + 16./x;

xi = linspace(a,b,N+2);
ye=yexact(xi);
figure;
plot(xi,abs(ye-z'),'linewidth',5);
legend('absolute error');
set(gca,'fontsize',15);
figure;
plot(xi,ye,xi,z);
legend('exact','numerical', 'fontsize',16);
maxerr = max(abs(ye-z'));
disp(["maxErr", maxerr]);
