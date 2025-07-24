%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERPOLATION PROBLEM

% Accepts vector of different lengths and values
x_pt = [2,4,7,15,18];
y_pt = [1,2,4,5,6];
x_pt_size = size(x_pt, 2);
y_pt_size = size(y_pt, 2);

% Randomly generate Z surface values
rng("default");
rand_range = [9,10];
rand_matrix = rand(x_pt_size, ...
                   y_pt_size, ...
                   "double");
z_pt = (rand_range(2)-rand_range(1)).*rand_matrix + rand_range(1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LINEAR SPACE DEFINITIONS

x_space_size = (max(x_pt) - min(x_pt))*10;
y_space_size = (max(y_pt) - min(y_pt))*10;
x_space = linspace(min(x_pt),max(x_pt),x_space_size);
y_space = linspace(min(y_pt),max(y_pt),y_space_size);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TENSOR PRODUCT INTERPOLATION

% Tensor Product Interpolation
% For 3 X pts
% l_i_deltaX = [(x_space - x_pt(2))/(x_pt(1)-x_pt(2)) .* (x_space - x_pt(3))/(x_pt(1)-x_pt(3));...
%               (x_space - x_pt(1))/(x_pt(2)-x_pt(1)) .* (x_space - x_pt(3))/(x_pt(2)-x_pt(3));...
%               (x_space - x_pt(1))/(x_pt(3)-x_pt(1)) .* (x_space - x_pt(2))/(x_pt(3)-x_pt(2))];

l_i_deltaX = zeros(x_pt_size, x_space_size);
for i=1:x_pt_size
    prod = 1;
    for k=1:x_pt_size
        if k ~= i
            prod = prod .* (x_space-x_pt(k))/(x_pt(i)-x_pt(k));
        end
    end
    l_i_deltaX(i,:) = prod;
end

l_j_deltaY = zeros(y_pt_size, y_space_size);
for j=1:y_pt_size
    prod = 1;
    for k=1:y_pt_size
        if k ~= j
            prod = prod .* (y_space-y_pt(k))/(y_pt(j)-y_pt(k));
        end
    end
    l_j_deltaY(j,:) = prod;
end

z_result = 0;
for i=1:x_pt_size
    for j=1:y_pt_size
        z_result = z_result + z_pt(i,j).*l_i_deltaX(i,:).*transpose(l_j_deltaY(j,:));
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VISUALISE

x_scatter = repmat(x_pt,1,y_pt_size);
y_scatter = [];
for m =1:y_pt_size
    y_scatter = [y_scatter, repmat(y_pt(m),1,x_pt_size)];
end
z_scatter = reshape(z_pt,1,x_pt_size*y_pt_size);
scatter3(x_scatter, y_scatter, z_scatter);
hold on;
scatter3([0], [0], [0], 0.001); % force 0 baseline

mesh(x_space, y_space, z_result);
