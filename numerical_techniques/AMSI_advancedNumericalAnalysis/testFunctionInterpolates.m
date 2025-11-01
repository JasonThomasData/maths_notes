x_pt = [-cos(pi/4),cos(pi/4)];
y_pt = [-cos(pi/4),cos(pi/4)];
x_pt_size = size(x_pt, 2);
y_pt_size = size(y_pt, 2);

z_pt = [8, 7; 1 -3];

x_space_size = 15%(max(x_pt) - min(x_pt))*10;
y_space_size = 15%(max(y_pt) - min(y_pt))*10;
x_space = linspace(min(x_pt),max(x_pt),x_space_size);
y_space = linspace(min(y_pt),max(y_pt),y_space_size);

x_scatter = repmat(x_pt,1,y_pt_size);
y_scatter = [];
for m =1:y_pt_size
    y_scatter = [y_scatter, repmat(y_pt(m),1,x_pt_size)];
end
z_scatter = reshape(z_pt,1,x_pt_size*y_pt_size);
scatter3(x_scatter, y_scatter, z_scatter);
hold on;
scatter3([0], [0], [0], 0.001); % force 0 baseline

disp(x_scatter)
disp(y_scatter)
disp(z_scatter)

u = cos(pi/4);
z_result = (1/(4*u^2))*(-3.*x_space.*transpose(y_space) - 17.*x_space.*u - 5.*transpose(y_space).*u + 13.*u.^2);

mesh(x_space, y_space, z_result);

M = [x_space.*transpose(y), x_space, transpose(y) 1; 
     x_space.*transpose(y), x_space, transpose(y) 1; 
     x_space.*transpose(y), x_space, transpose(y) 1;
     x_space.*transpose(y), x_space, transpose(y) 1];
M*[1; 1; 1; 1]