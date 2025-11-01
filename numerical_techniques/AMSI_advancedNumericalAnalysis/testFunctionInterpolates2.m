x_pt = [-1,0,1,0];
y_pt = [0,-1,0,1];
x_pt_size = size(x_pt, 2);
y_pt_size = size(y_pt, 2);

z_pt = [8, 7, 1, -3];

x_space_size = 15%(max(x_pt) - min(x_pt))*10;
y_space_size = 15%(max(y_pt) - min(y_pt))*10;
x_space = linspace(min(x_pt),max(x_pt),x_space_size);
y_space = linspace(min(y_pt),max(y_pt),y_space_size);

scatter3(x_pt, y_pt, z_pt);
hold on;
scatter3([0], [0], [0], 0.001); % force 0 baseline

disp(x_pt)
disp(y_pt)
disp(z_pt)

u = cos(pi/4);
%z_result = ((5/(4*u) + 3/4).*x_space.*transpose(y_space) + (-5/(4*u) + 3/4).*x_space + (-17/(4*u)).*transpose(y_space) + (13/4));
z_result = (13/4).*(x_space.^2 - transpose(y_space).^2) + (-22/4).*x_space + (-12/4).*transpose(y_space) + (3/4);

%z_result = ((2.5178).*x_space.*transpose(y_space) + (-1.0178).*x_space + (-6.0104).*transpose(y_space) + (3.25));

mesh(x_space, y_space, z_result);

R_inv = [cos(pi/4), sin(pi/4), 0 , 0;
         -sin(pi/4), cos(pi/4), 0, 0;
         0, 0, 1, 0;
         0, 0, 0, 1];

format rat;

V = [5/(4*u^2);
     3/(4*u);
     -17/(4*u);
     13/4];
disp(R_inv*V)