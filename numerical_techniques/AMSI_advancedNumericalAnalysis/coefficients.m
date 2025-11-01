syms x y

xi_0(x) = (2*x^2 - 3*x + 1);
xi_1(x) = (-4*x^2 +4*x);
xi_2(x) = (2*x^2 - x);

eta_0(y) = (2*y^2 - 3*y + 1);
eta_1(y) = (-4*y^2 +4*y);
eta_2(y) = (2*y^2 - y);

phi = {@(x,y) xi_0(x)*eta_0(y);
       @(x,y) xi_2(x)*eta_0(y);
       @(x,y) xi_2(x)*eta_2(y);
       @(x,y) xi_0(x)*eta_2(y);
       @(x,y) xi_1(x)*eta_0(y);
       @(x,y) xi_2(x)*eta_1(y);
       @(x,y) xi_1(x)*eta_2(y);
       @(x,y) xi_0(x)*eta_1(y);
       @(x,y) xi_1(x)*eta_1(y)};

for i=1:9
    phi_i_def_x = int(phi(i), x, [0 1]);
    phi_i_def_xy = int(phi_i_def_x, y, [0 1]);
    disp(phi_i_def_xy);
end