x = [1, -4,   0];
y = [3, 13, -23];

scatter(x, y);
hold on;

% Vandermonde matrix
V = [x(1)^0, x(1)^1, x(1)^2;
     x(2)^0, x(2)^1, x(2)^2;
     x(3)^0, x(3)^1, x(3)^2];
v_coef = V\y';

% Newton interpolation
% N = [x(1)^0, (x(1)-x(1)), (x(1)-x(1))*(x(1)-x(2));
%      x(2)^0, (x(2)-x(1)), (x(2)-x(1))*(x(2)-x(2));
%      x(3)^0, (x(3)-x(1)), (x(3)-x(1))*(x(3)-x(2))];
% Is equal to
N = [x(1)^0,           0,                       0;
     x(2)^0, (x(2)-x(1)),                       0;
     x(3)^0, (x(3)-x(1)), (x(3)-x(1))*(x(3)-x(2))];

n_coef = N\y';

x_space = linspace(-4,1,100);
v_curve = v_coef(1) +        v_coef(2)*x_space +                    v_coef(3)*x_space.^2;
n_curve = n_coef(1) + n_coef(2).*(x_space-x(1)) + n_coef(3).*(x_space-x(1)).*(x_space-x(2));

% TODO: find examples where one is better than the other.
plot(x_space, v_curve);
plot(x_space, n_curve);
