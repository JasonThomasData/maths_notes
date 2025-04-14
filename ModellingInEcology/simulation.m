% Generalized Lotka-Volterra Simulation to Equilibrium using Forward Euler
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


% Initial abundances (positive starting values)
N = [1; 1; 1; 1; 1]; 


% Time parameters
dt = 0.01;        % Time step
Tmax = 500;       % Max simulation time
threshold = 1e-6; % Convergence threshold


% Preallocate for recording (optional)
record = zeros(length(N), floor(Tmax/dt));
time = zeros(1, floor(Tmax/dt));


% Simulation loop
for t = 1:floor(Tmax/dt)
    growth = alpha + beta * N;        % Interaction term
    dN = N .* growth;                % Lotka-Volterra differential
    N_new = N + dt * dN;             % Euler update
    
    % Prevent negative values (optional, for biological realism)
    N_new(N_new < 0) = 0; 
    
    % Record
    record(:, t) = N_new;
    time(t) = t * dt;
    
    % Check for convergence
    if max(abs(N_new - N)) < threshold
        fprintf('Converged at time %.2f\n', t * dt);
        break;
    end
    
    % Update for next step
    N = N_new;
end


% Plot results
figure;
plot(time(1:t), record(:, 1:t));
xlabel('Time');
ylabel('Abundance');
legend('Species 1', 'Species 2', 'Species 3', 'Species 4', 'Species 5');
title('GLV System Simulated to Equilibrium');

% Note that the first column should match the numerical simulation
disp("Inverse of Jacobian or Community Matrix");
disp(-inv(beta));
