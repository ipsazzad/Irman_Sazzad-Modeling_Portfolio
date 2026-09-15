% 1. Parameters
parameters beta gamma alpha delta rho_A sigma_A;
beta    = 0.97;
gamma   = 0.40;
alpha   = 0.35;
delta   = 0.06;
rho_A   = 0.95;
sigma_A = 0.01;

% 2. Endogenous variables
var c l k y i A z lambda;

% 3. Exogenous shock
varexo eps_A;

% 4. Model equations
model;
    % TFP (log form)
    z = rho_A*z(-1) + eps_A;
    A = exp(z);
    % Production function
    y = A * k(-1)^alpha * l^(1-alpha);
    % Resource constraint
    y = c + i;
    % Capital accumulation
    k = (1-delta)*k(-1) + i;
    % FOC: consumption
    lambda = gamma/c;
    % FOC: labor
    (1-gamma)/(1-l) = lambda*(1-alpha)*y/l;
    % Euler equation
    lambda = beta*lambda(+1)*( alpha*y(+1)/k + 1-delta );
end;

% 5. Initial values
initval;
    z      = 0;
    A      = 1;
    l      = 0.36;
    k      = 2.8;
    y      = 0.75;
    c      = 0.57;
    i      = 0.18;
    lambda = 0.7;
end;
steady;
check;

% 6. Shock variance
shocks;
    var eps_A = sigma_A^2;
end;

% 7. Simulation
stoch_simul(order=1, irf=40);