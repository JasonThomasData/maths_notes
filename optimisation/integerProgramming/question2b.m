function [Aeq, beq, A, b] = question2b(CostL)

    % It's neater to make inequalities for pairs of locations 
    % and then compose them at the end into their final matrix form
    
    % constr a)
    % = 0
    AirleeBeachGoldCoastEq = [eye(4), zeros([4,4]), -eye(4), zeros([4,12])];
    
    % constr b)
    % <= 0
    BrisbaneSydneyIneq = [zeros([1,4]),1,zeros([1,6]),-1,zeros(1,12)];
    
    % constr c)
    % = 0
    MelbourneSydneyEq = [zeros([1,14]), ...
                         ones([1,2]), ...
                         zeros([1,2]), ...
                         ones([1,2]), ...
                         zeros([1,4])];
    
    % constr d)
    % <= 1
    BrisbanePerthIneq = [zeros([4,4]), eye(4), zeros([4,12]), eye(4)];
    
    % constr e)
    % <= 300
    Year1Ineq = repmat([1,zeros([1,3])],[1,6]) .* CostL;
    
    % constr f)
    % This one was added to force at most one project per location
    
    years=4;
    locations=6;
    
    UniqueLocationIneq = zeros(locations, years*locations);
    
    for i=1:locations
        newRow = [repmat(zeros([1,4]), [1,i-1]), ...
                         ones([1,4]), ...
                         repmat(zeros([1,4]), [1,locations-i])];
        UniqueLocationIneq(i,:) = newRow;
    end
    
    % Now, combine all results
    
    Aeq = [AirleeBeachGoldCoastEq;
           MelbourneSydneyEq;
           ones([1,24])];
    
    beq = [zeros([5,1,]);
           4];
    

    A = [BrisbaneSydneyIneq;
         BrisbanePerthIneq;
         Year1Ineq;
         UniqueLocationIneq];
    b = [0;
         ones([4,1]);
         300;
         ones(6,1)];

end
