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
Tmax = 60;       % Max simulation time


% Preallocate for recording (optional)
record = zeros(length(N), floor(Tmax/dt));
time = zeros(1, floor(Tmax/dt));

% This is the speices that I'm going to erradicate
errad_sp = 1;

% Simulation loop
for t = 1:floor(Tmax/dt)
    growth = alpha + beta * N;        % Interaction term
    dN = N .* growth;                % Lotka-Volterra differential

    N_new = N + dt * dN;             % Euler update

    if t*dt == 40
        N_eq = N;
    elseif t*dt > 40
        N_new(errad_sp) = 0;
    end
    
    % Prevent negative values (optional, for biological realism)
    N_new(N_new < 0) = 0;
    
    % Record
    record(:, t) = N_new;
    time(t) = t * dt;
    
    % Update for next step
    N = N_new;
end

% Treat this as true

% We expect the signs not to match. Press perturbations are a poor
% indication of species eradication.

disp("Sign of numerical simulation result");
eradication_pred_numerical = sign(N - N_eq);
disp(eradication_pred_numerical);

disp("Sign of -inv(beta) simulation result");
press_pred_analytic = sign(-inv(beta));
disp(press_pred_analytic(:,errad_sp));


% Plot results
figure(1), clf;
for i = 1:5
    subplot(3,2,i);
    pp = plot(time(1:t), record(i, 1:t),'linewidth',2);
    if eradication_pred_numerical(i) > 0
        set(pp, 'color', 'b');
    else
        set(pp, 'color', 'r');
    end
    xlabel('Time');
    ylabel('Abundance');
    title(['Species ' num2str(i)]);
    xlim([20 60])
end


subplot(3,2,6);
plot(time(1:t), record(:, 1:t),'linewidth',2);
xlabel('Time');
ylabel('Abundance');
title(['Species ' num2str(i)]);
xlim([1 50])
