function [A, b] = LinearConstraints()

    % A * rx <= b, for the upper bounds
    A_UB = [[eye(3); eye(3)], eye(6)];
    b_UB = [5;5;5;3;3;3];

    A_LB = [[eye(3); eye(3)], -eye(6)];
    b_LB = zeros(6,1);

    A = [A_UB; A_LB];
    b = [b_UB; b_LB];
end