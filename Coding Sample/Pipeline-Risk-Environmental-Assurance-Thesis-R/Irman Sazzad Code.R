# LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

# PARAMETERS
params_base <- list(
  c_L = 1000,
  c_H = 2000,
  M = 200000,
  lambda = 1000000,
  q0 = 0.05,
  alpha = 0.2,
  gamma = 0.5,
  R = 18000000
)

# FUNCTIONS
q_func <- function(e, q0, alpha) q0 * exp(-alpha * e)
dq_func <- function(e, q0, alpha) -alpha * q0 * exp(-alpha * e)

solve_effort <- function(B, c, q0, alpha) {
  effort_eq <- function(e) dq_func(e, q0, alpha) * B + 2 * c * e
  uniroot(effort_eq, lower = 1e-6, upper = 100)$root
}

landowner_util <- function(e, lambda, M, q0, alpha) {
  q <- q_func(e, q0, alpha)
  q * sqrt(lambda - M) + (1 - q) * sqrt(lambda)
}

agent_util <- function(e, c, B, R, q0, alpha) {
  R - c * e^2 - q_func(e, q0, alpha) * B
}

objective <- function(B_vec, params) {
  with(params, {
    B_L <- B_vec[1]; B_H <- B_vec[2]
    e_L <- solve_effort(B_L, c_L, q0, alpha)
    e_H <- solve_effort(B_H, c_H, q0, alpha)
    IC1 <- agent_util(e_L, c_L, B_L, R, q0, alpha) - agent_util(e_H, c_L, B_H, R, q0, alpha)
    IR2 <- agent_util(e_H, c_H, B_H, R, q0, alpha)
    penalty <- 0
    if (IC1 < 0) penalty <- penalty + 1 / (abs(IC1) + 1e-6)
    if (IR2 < 0) penalty <- penalty + 1 / (abs(IR2) + 1e-6)
    if (abs(B_H - B_L) < 10000) penalty <- penalty + 10000
    EU <- gamma * landowner_util(e_L, lambda, M, q0, alpha) +
      (1 - gamma) * landowner_util(e_H, lambda, M, q0, alpha)
    return(-EU + penalty)
  })
}

# Calculate q(e) at optimal effort levels (baseline parameters)
q_L <- q_func(0.6576, params_base$q0, params_base$alpha)
q_H <- q_func(0.7527, params_base$q0, params_base$alpha)

cat(sprintf("Probability of leak at optimal effort:\n Low-cost agent: %.5f\n High-cost agent: %.5f\n", q_L, q_H))

# COMPARATIVE STATICS: Damage (M)
M_values <- seq(100000, 300000, by = 25000)
df_sensitivity <- data.frame()

for (M_val in M_values) {
  params <- params_base
  params$M <- M_val
  result <- optim(par = c(150000, 350000), fn = objective, params = params,
                  method = "L-BFGS-B", lower = c(100000, 200000), upper = c(300000, 500000))
  B_L <- result$par[1]; B_H <- result$par[2]
  e_L <- solve_effort(B_L, params$c_L, params$q0, params$alpha)
  e_H <- solve_effort(B_H, params$c_H, params$q0, params$alpha)
  EU <- params$gamma * landowner_util(e_L, params$lambda, params$M, params$q0, params$alpha) +
    (1 - params$gamma) * landowner_util(e_H, params$lambda, params$M, params$q0, params$alpha)
  df_sensitivity <- rbind(df_sensitivity, data.frame(
    M = M_val, B_L = B_L, B_H = B_H, e_L = e_L, e_H = e_H, EU = EU
  ))
}
df_long <- pivot_longer(df_sensitivity, cols = c(B_L, B_H, e_L, e_H, EU), names_to = "Variable", values_to = "Value")

p1 <- ggplot(df_long %>% filter(Variable %in% c("B_L", "B_H")), aes(x = M, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Bond", x = "Damage (M)") +
  ylim(120000, 380000) + theme_minimal() + theme(plot.title = element_blank())

p2 <- ggplot(df_long %>% filter(Variable %in% c("e_L", "e_H")), aes(x = M, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Effort", x = "Damage (M)") +
  ylim(0.6, 0.8) + theme_minimal() + theme(plot.title = element_blank())

p3 <- ggplot(df_long %>% filter(Variable == "EU"), aes(x = M, y = Value)) +
  geom_line(color = "darkgreen") + geom_point(color = "darkgreen") +
  labs(y = "EU", x = "Damage (M)") +
  ylim(991, 1000) + theme_minimal() + theme(plot.title = element_blank())

grid.arrange(p1, p2, p3, ncol = 3, top = "Comparative Statics involving Damage (M)")

# COMPARATIVE STATICS: High-Cost Agent (c_H)
c_H_values <- seq(1500, 3000, by = 250)
df_cH <- data.frame()
for (c_H_val in c_H_values) {
  params <- params_base
  params$c_H <- c_H_val
  result <- optim(par = c(150000, 350000), fn = objective, params = params,
                  method = "L-BFGS-B", lower = c(100000, 200000), upper = c(300000, 500000))
  B_L <- result$par[1]; B_H <- result$par[2]
  e_L <- solve_effort(B_L, params$c_L, params$q0, params$alpha)
  e_H <- solve_effort(B_H, params$c_H, params$q0, params$alpha)
  EU <- params$gamma * landowner_util(e_L, params$lambda, params$M, params$q0, params$alpha) +
    (1 - params$gamma) * landowner_util(e_H, params$lambda, params$M, params$q0, params$alpha)
  df_cH <- rbind(df_cH, data.frame(c_H = c_H_val, B_L = B_L, B_H = B_H, e_L = e_L, e_H = e_H, EU = EU))
}

df_long <- pivot_longer(df_cH, cols = c(B_L, B_H, e_L, e_H, EU), names_to = "Variable", values_to = "Value")

p1 <- ggplot(df_long %>% filter(Variable %in% c("B_L", "B_H")), aes(x = c_H, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Bond", x = "c_H") +
  ylim(120000, 380000) + theme_minimal() + theme(plot.title = element_blank())

p2 <- ggplot(df_long %>% filter(Variable %in% c("e_L", "e_H")), aes(x = c_H, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Effort", x = "c_H") +
  ylim(0.5, 1) + theme_minimal() + theme(plot.title = element_blank())

p3 <- ggplot(df_long %>% filter(Variable == "EU"), aes(x = c_H, y = Value)) +
  geom_line(color = "darkgreen") + geom_point(color = "darkgreen") +
  labs(y = "EU", x = "c_H") +
  ylim(995, 996) + theme_minimal() + theme(plot.title = element_blank())

grid.arrange(p1, p2, p3, ncol = 3, top = "Comparative Statics involving High-Cost Agents (c_H)")

# COMPARATIVE STATICS: Low-Cost Effort Cost (c_L)
c_L_values <- seq(750, 2500, by = 250)
df_cL <- data.frame()
for (c_L_val in c_L_values) {
  params <- params_base
  params$c_L <- c_L_val
  result <- optim(par = c(150000, 350000), fn = objective, params = params,
                  method = "L-BFGS-B", lower = c(100000, 200000), upper = c(300000, 500000))
  B_L <- result$par[1]; B_H <- result$par[2]
  e_L <- solve_effort(B_L, params$c_L, params$q0, params$alpha)
  e_H <- solve_effort(B_H, params$c_H, params$q0, params$alpha)
  EU <- params$gamma * landowner_util(e_L, params$lambda, params$M, params$q0, params$alpha) +
    (1 - params$gamma) * landowner_util(e_H, params$lambda, params$M, params$q0, params$alpha)
  df_cL <- rbind(df_cL, data.frame(c_L = c_L_val, B_L = B_L, B_H = B_H, e_L = e_L, e_H = e_H, EU = EU))
}

df_long <- pivot_longer(df_cL, cols = c(B_L, B_H, e_L, e_H, EU), names_to = "Variable", values_to = "Value")

p1 <- ggplot(df_long %>% filter(Variable %in% c("B_L", "B_H")), aes(x = c_L, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Bond", x = "c_L") +
  ylim(120000, 380000) + theme_minimal() + theme(plot.title = element_blank())

p2 <- ggplot(df_long %>% filter(Variable %in% c("e_L", "e_H")), aes(x = c_L, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Effort", x = "c_L") +
  ylim(0.2, 1) + theme_minimal() + theme(plot.title = element_blank())

p3 <- ggplot(df_long %>% filter(Variable == "EU"), aes(x = c_L, y = Value)) +
  geom_line(color = "darkgreen") + geom_point(color = "darkgreen") +
  labs(y = "EU", x = "c_L") +
  ylim(995, 996) + theme_minimal() + theme(plot.title = element_blank())

grid.arrange(p1, p2, p3, ncol = 3, top = "Comparative Statics involving Low-Cost Agents (c_L)")

# COMPARATIVE STATICS: Agent Revenue (R)
R_values <- seq(16000000, 20000000, by = 500000)
df_R <- data.frame()
for (R_val in R_values) {
  params <- params_base
  params$R <- R_val
  result <- optim(par = c(150000, 350000), fn = objective, params = params,
                  method = "L-BFGS-B", lower = c(100000, 200000), upper = c(300000, 500000))
  B_L <- result$par[1]; B_H <- result$par[2]
  e_L <- solve_effort(B_L, params$c_L, params$q0, params$alpha)
  e_H <- solve_effort(B_H, params$c_H, params$q0, params$alpha)
  EU <- params$gamma * landowner_util(e_L, params$lambda, params$M, params$q0, params$alpha) +
    (1 - params$gamma) * landowner_util(e_H, params$lambda, params$M, params$q0, params$alpha)
  df_R <- rbind(df_R, data.frame(R = R_val, B_L = B_L, B_H = B_H, e_L = e_L, e_H = e_H, EU = EU))
}

df_long <- pivot_longer(df_R, cols = c(B_L, B_H, e_L, e_H, EU), names_to = "Variable", values_to = "Value")

p1 <- ggplot(df_long %>% filter(Variable %in% c("B_L", "B_H")), aes(x = R, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Bond", x = "R") +
  ylim(120000, 380000) + theme_minimal() + theme(plot.title = element_blank())

p2 <- ggplot(df_long %>% filter(Variable %in% c("e_L", "e_H")), aes(x = R, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Effort", x = "R") +
  ylim(0.5, 1) + theme_minimal() + theme(plot.title = element_blank())

p3 <- ggplot(df_long %>% filter(Variable == "EU"), aes(x = R, y = Value)) +
  geom_line(color = "darkgreen") + geom_point(color = "darkgreen") +
  labs(y = "EU", x = "R") +
  ylim(990, 1000) + theme_minimal() + theme(plot.title = element_blank())

grid.arrange(p1, p2, p3, ncol = 3, top = "Comparative Statics involving Revenue")

# COMPARATIVE STATICS: Landowner Wealth (Lambda)
lambda_values <- seq(800000, 1200000, by = 50000)
df_lambda <- data.frame()

for (lambda_val in lambda_values) {
  params <- params_base
  params$lambda <- lambda_val
  result <- optim(par = c(150000, 350000), fn = objective, params = params,
                  method = "L-BFGS-B", lower = c(100000, 200000), upper = c(300000, 500000))
  B_L <- result$par[1]; B_H <- result$par[2]
  e_L <- solve_effort(B_L, params$c_L, params$q0, params$alpha)
  e_H <- solve_effort(B_H, params$c_H, params$q0, params$alpha)
  EU <- params$gamma * landowner_util(e_L, params$lambda, params$M, params$q0, params$alpha) +
    (1 - params$gamma) * landowner_util(e_H, params$lambda, params$M, params$q0, params$alpha)
  df_lambda <- rbind(df_lambda, data.frame(lambda = lambda_val, B_L = B_L, B_H = B_H, e_L = e_L, e_H = e_H, EU = EU))
}

df_long <- pivot_longer(df_lambda, cols = c(B_L, B_H, e_L, e_H, EU), names_to = "Variable", values_to = "Value")

p1 <- ggplot(df_long %>% filter(Variable %in% c("B_L", "B_H")), aes(x = lambda, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Bond", x = "λ") +
  ylim(120000, 380000) + theme_minimal() + theme(plot.title = element_blank())

p2 <- ggplot(df_long %>% filter(Variable %in% c("e_L", "e_H")), aes(x = lambda, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Effort", x = "λ") +
  ylim(0.5, 1) + theme_minimal() + theme(plot.title = element_blank())

p3 <- ggplot(df_long %>% filter(Variable == "EU"), aes(x = lambda, y = Value)) +
  geom_line(color = "darkgreen") + geom_point(color = "darkgreen") +
  labs(y = "EU", x = "λ") +
  ylim(880, 1100) + theme_minimal() + theme(plot.title = element_blank())

grid.arrange(p1, p2, p3, ncol = 3, top = "Comparative Statics involving Landowner Wealth")

# COMPARATIVE STATICS: Distribution of Operators' cost (Gamma)
gamma_values <- seq(0.1, 0.9, by = 0.1)
df_gamma <- data.frame()

for (gamma_val in gamma_values) {
  params <- params_base
  params$gamma <- gamma_val
  result <- optim(par = c(150000, 350000), fn = objective, params = params,
                  method = "L-BFGS-B", lower = c(100000, 200000), upper = c(300000, 500000))
  B_L <- result$par[1]; B_H <- result$par[2]
  e_L <- solve_effort(B_L, params$c_L, params$q0, params$alpha)
  e_H <- solve_effort(B_H, params$c_H, params$q0, params$alpha)
  EU <- params$gamma * landowner_util(e_L, params$lambda, params$M, params$q0, params$alpha) +
    (1 - params$gamma) * landowner_util(e_H, params$lambda, params$M, params$q0, params$alpha)
  df_gamma <- rbind(df_gamma, data.frame(gamma = gamma_val, B_L = B_L, B_H = B_H, e_L = e_L, e_H = e_H, EU = EU))
}

df_long <- pivot_longer(df_gamma, cols = c(B_L, B_H, e_L, e_H, EU), names_to = "Variable", values_to = "Value")

p1 <- ggplot(df_long %>% filter(Variable %in% c("B_L", "B_H")), aes(x = gamma, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Bond", x = "γ") +
  ylim(120000, 380000) + theme_minimal() + theme(plot.title = element_blank())

p2 <- ggplot(df_long %>% filter(Variable %in% c("e_L", "e_H")), aes(x = gamma, y = Value, color = Variable)) +
  geom_line() + geom_point() + labs(y = "Effort", x = "γ") +
  ylim(0.5, 1) + theme_minimal() + theme(plot.title = element_blank())

p3 <- ggplot(df_long %>% filter(Variable == "EU"), aes(x = gamma, y = Value)) +
  geom_line(color = "darkgreen") + geom_point(color = "darkgreen") +
  labs(y = "EU", x = "γ") +
  ylim(995, 996) + theme_minimal() + theme(plot.title = element_blank())

grid.arrange(p1, p2, p3, ncol = 3, top = "Comparative Statics involving Cost Operator Share")

# Effort vs Bond levels for low-cost and high-cost agents
B_seq <- seq(100000, 400000, by = 10000)
effort_L <- sapply(B_seq, function(B) solve_effort(B, params_base$c_L, params_base$q0, params_base$alpha))
effort_H <- sapply(B_seq, function(B) solve_effort(B, params_base$c_H, params_base$q0, params_base$alpha))
df_effort_bond <- data.frame(
  Bond = rep(B_seq, 2),
  Effort = c(effort_L, effort_H),
  Type = rep(c("Low-cost agent", "High-cost agent"), each = length(B_seq))
)
p_effort_bond <- ggplot(df_effort_bond, aes(x = Bond, y = Effort, color = Type)) +
  geom_line(size=1) + geom_point() +
  labs(y = "Optimal Effort", x = "Bond Level (B)") +
  ylim(0, max(df_effort_bond$Effort) * 1.1) +
  ggtitle("Optimal Effort as a Function of Bond Levels") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(p_effort_bond)
