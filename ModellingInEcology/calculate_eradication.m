% Generalized Lotka-Voltera Simulation to Equilibrium using Forward Euler
clear all;

% Parameters
alpha = [0.78; 0.38; 0.75; 0.18; 0.33];
beta = [
   -0.04,  0,     0,    0.02,  0.07;
    0.18, -0.17, -0.13, -0.16, 0.05;
   -0.11,  0.08, -0.05, -0.12, 0.19;
    0,    -0.06,  0.13, -0.02, 0;
    0.01,  0,     0.05, -0.05, -0.09
];

% Steady state, found without simulation
N_eq = -inv(beta)*alpha;
scaled_beta = beta * diag(N_eq);
% We expect negative real parts here, for state to be steady
disp(eigs(jacobian));

% https://nadiah.org/2025/03/01/eradication

% This is the speices that I'm going to eradicate
erad_sp = 2;

alpha_er = alpha(setdiff(1:end, erad_sp));
beta_er = beta(setdiff(1:end,erad_sp),setdiff(1:end,erad_sp));
jacobian_er = jacobian(setdiff(1:end,erad_sp),setdiff(1:end,erad_sp));
er_effect = jacobian(setdiff(1:end,erad_sp),erad_sp);
N_er = N_eq(setdiff(1:end, erad_sp));

jacobian_er*er_effect

p = -inv(beta_er)*alpha_er
p - N_er