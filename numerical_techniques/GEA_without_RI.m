function x = GEA_without_RI(A,b)
    % PROVIDED BY LECTURER
    
    % This function solves a system of n linear equations
    % with n unknown variables, given in matrix form A*x = b.
    % This function implements a raw version of the Gaussian elimination algorithm,
    % which assumes that all pivot elements encountered are not zero,
    % and thus there is no need in row interchanges.
    %
    % Input : A, an n by n matrix,
    % : b, a vector of length n.
    %
    % Output : x, solution of A*x = b.
    function ProcessPivotElement
        % if pivot element is zero, then report a failure and quit.
        if abs(A(k,k)) < 10^(-10),
            error(’GEA without row interchanges failed: a pivot element is zero’);
        end
    end
    
    [m,n] = size(A);

    if m~=n
        error(’The number of equations must be equal to the number of variables.’);
    end

    m = length(b);

    if m~=n
        error(’The length of the second argument must be equal to the size of the first argument.’);
    end
    
    % Forward phase of the Gaussian elimination algorithm
    for k = 1:n-1
        ProcessPivotElement;
        for i=k+1:n
            m = A(i,k)/A(k,k); % calculate pivoting factor
            A(i,:)=A(i,:) - m*A(k,:); % perform ERO of type~I
            b(i)=b(i) - m*b(k); % (the extended part of the matrix)
        end
    end

    % Backward phase of the Gaussian elimination algorithm
    for k=n:-1:2
        for i=k-1:-1:1
            m = A(i,k)/A(k,k); % calculate pivoting factor
            A(i,:)=A(i,:) - m*A(k,:); % perform ERO of type~I
            b(i)=b(i) - m*b(k);
        end
    end

    % At this stage the matrix A is diagonal.

    x=zeros(n,1); % x is initialised as zero column.

    for i=1:n
        x(i)=b(i)/A(i,i); % perform ERO of type II
    end
end
