function y = RK2_Autonomous(f,init,h,b)

    n = abs(b-init(1))/h;

    y=zeros(length(init),round(n+1)); % matlab was complaining about floats, so round inf zeros
    y(:,1) = init;

    disp(y)

    for i=1:n+1
    %    substituted = [subs(f(2),x,y(1,i));
        y(i+1) = y(:,i) + h*f(i);
    end
end
