x_1 = [1;
       3;
       2.2];
y_1 = [1.9;
       2.2;
       0.1];
z_1 = [2.5;
       2;
       0.9];
M_1 = [x_1, y_1, ones(3,1)];

coeffs_1 = M_1\z_1;

x_2 = [1;
       3;
       1.9];
y_2 = [1.9;
       2.2;
       4];
z_2 = [2.5;
       2;
       1.9];

M_2 = [x_2, y_2, ones(3,1)];

coeffs_2 = M_2\z_2;

x_linear_space = linspace(0,4,200);
y_linear_space = linspace(0,4,200);
[cart_x, cart_y] = meshgrid(x_linear_space, y_linear_space);
x_cart = cart_x(:);
y_cart = cart_y(:);

in_poly_1 = inpolygon(x_cart,y_cart, x_1, y_1);
in_poly_2 = inpolygon(x_cart,y_cart, x_2, y_2);

z_pts_poly_1 = coeffs_1(1).*x_cart(in_poly_1) + coeffs_1(2).*y_cart(in_poly_1) + coeffs_1(3);
z_pts_poly_2 = coeffs_2(1).*x_cart(in_poly_2) + coeffs_2(2).*y_cart(in_poly_2) + coeffs_2(3);
plot3(x_cart(in_poly_1), y_cart(in_poly_1), z_pts_poly_1, 'linestyle','none','marker','.');
hold on;
plot3(x_cart(in_poly_2), y_cart(in_poly_2), z_pts_poly_2, 'linestyle','none','marker','.');
hold on;
plot3(0,0,0);

%z_surface_1 = coeffs_1(1).*x_linear_space + coeffs_1(2).*transpose(y_linear_space) + coeffs_1(3);
%mesh(x_linear_space, y_linear_space, z_surface_1);
