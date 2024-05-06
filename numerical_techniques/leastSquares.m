function a = leastSquares(x, y, n)
    usage = "Fits an approximation to data using the least squares technique. Example use: \n x=[1,2,3] \n y=[2.5,3,5] \n n=2 \n leastSquares(x,y,n)";

    if n > length(x)-1 || ...
        ~isnumeric(x) || ...
        ~isnumeric(y) || ...
        length(x) ~= length(y)
        error(sprintf(usage))
    end

    s = zeros(1,(2*n)+1);

    for k=0:(2*n)
        s(1,k+1) = sum(x.^k);
    end

    S = zeros(n+1,n+1);
    for i=1:(n+1)
        for j=1:(n+1)
            S(i,j) = s(i+j-1);
        end
    end

    b = zeros(n,1);
    for k=0:n
        b(k+1,1) = sum(x.^k .* y);
    end

    a = S\b;
end
