function [x, y] = RK4_Autonomous(f,x0,y0,h,b)
    % This file borrows a lot from the matlab handbook

    n = abs(b-x0)/h;
    if b>x0
        x=x0+h*(0:n);
    else
        x=x0+h*(0:-1:-n);
    end
    
    y=zeros(1,n+1);
    y(1) = y0;
    
    for i=1:n
        k1 = h*f(y(i));
        g2 = h*f(y(i)+k1/2);
        g3 = h*f(y(i)+k2/2);
        g4 = h*f(y(i)+k3);
        y(i+1) = y(i) + (k1+2*k2+2*k3+k4)/6;
    end
end
