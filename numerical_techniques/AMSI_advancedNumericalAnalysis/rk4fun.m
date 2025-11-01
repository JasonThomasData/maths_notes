function [tn,yn]=rk4fun(f,ti,tf,yi,n)
    %Use explicit fourth order Runge-Kutta method for ode y'=f(t,y)
    %It also solves a system of ODEs
     %Input:
      %    ti and tf: initial and final time 
      %    yi: the initial value given at time ti
      %    n: the number of all time steps
      %    f: the right hand side funtion as anonymous function 
      %    f should be a function of t and y 
      %Example of f: f= @(t,y)(2*y+t);
     %Output: 
      %   yn: solution of the IVP (matrix of size n by m)
      %   tn: the discrete time steps where the solution is approximated
      
      %  Note that n is the number of time-steps and m is 
      %  the number of components in the system 
      
    %Example with two components:   
    %ti=0;tf=4;yi=[1;3];n=1000;f=@(t,y)([-6*y(1);y(2)]);  
    %Example with one component: 
    %ti=0;tf=4;yi=5;n=1000;f=@(t,y)(y+t^2);  
    %Example with three components: 
    %ti=0;tf=4;yi=[1;4;0.5];n=1000;f=@(t,y)([y(1);t*y(2);t^2-y(3)]);  

  h=(tf-ti)/(n-1);
  d=length(yi); 
  %the initial value of the ode determine the
  %dimension of the system given as a row vector
  yn=zeros(d,n);
  yn(:,1)=yi;
  tn=linspace(ti,tf,n)'; 
  %start the iteration of Runge-Kutta method
  for i=1:n-1
      K1=f(tn(i),yn(:,i)); 
      K2=f(tn(i)+h/2,yn(:,i)+h/2*K1);
      K3=f(tn(i)+h/2,yn(:,i)+h/2*K2);
      K4=f(tn(i)+h,yn(:,i)+h*K3);
      yn(:,i+1)=yn(:,i)+h/6*(K1+2*K2+2*K3+K4);
  end
  yn=yn';
end
