library(tidyverse)
library(dplyr)
library(readr)
library(broom)
library(knitr)

setwd("E:/Study/Fall 25/Metrix/PS/3/")
loan <- read.csv("loanmain.csv")
logf <- file("results_log.txt", open = "wt")
sink(logf, type = "output")

                             ## Question 5a

# Filtering required data
loan_sub <- loan %>%
  filter(round %in% c(1, 2)) %>%
  filter(!is.na(type) & !is.na(retail) & !is.na(age) & !is.na(gender) &
           !is.na(total_profit) & !is.na(lnlabor) & !is.na(allloan_end) &
           !is.na(shutdownall) & !is.na(education_college))

# Creating Round 1 subsets
r1 <- loan_sub %>% filter(round == 1)
r1_0 <- r1 %>% filter(type == 0)
r1_1 <- r1 %>% filter(type == 1)

# Mean calculator function
get_mean <- function(df) {
  df %>%
    summarise(
      treat = mean(type),
      retail = mean(retail),
      age = mean(age),
      gender = mean(gender),
      college = mean(education_college),
      profit = mean(total_profit),
      lnemp = mean(lnlabor),
      loan_amt = mean(allloan_end),
      shutdown = mean(shutdownall)
    )
}

# Computing means
m_r1 <- get_mean(r1)
m_r1_0 <- get_mean(r1_0)
m_r1_1 <- get_mean(r1_1)

# Table 1
table1 <- data.frame(
  variable = c("Treatment Status", "Retail Industry", "Age",
               "Gender", "College Education", "Total Profit",
               "Log Employees", "Firm Borrowing", "Firm Shutdown"),
  round1 = as.numeric(m_r1),
  round1_type0 = as.numeric(m_r1_0),
  round1_type1 = as.numeric(m_r1_1)
)

# Output table
cat("Table 1: Summary Statistics (Round 1)\n")
print(table1)

                                  ## Question 5b

# Round 2 data
r2 <- loan_sub %>% filter(round == 2)

# Running the 4 regressions
mod1 <- lm(total_profit ~ type + retail + age + gender + education_college, data = r2)
mod2 <- lm(lnlabor ~ type + retail + age + gender + education_college, data = r2)
mod3 <- lm(allloan_end ~ type + retail + age + gender + education_college, data = r2)
mod4 <- lm(shutdownall ~ type + retail + age + gender + education_college, data = r2)

# Extracting coefficient estimates only
get_est <- function(mod) {
  as.numeric(coef(mod))
}

# variable names
var_names <- names(coef(mod1))

# Table 2
table2 <- data.frame(
  Variable = var_names,
  Total_Profit = get_est(mod1),
  Log_Employees = get_est(mod2),
  Firm_Borrowing = get_est(mod3),
  Shutdown = get_est(mod4)
)

# Appending number of observations
table2 <- rbind(
  table2,
  data.frame(
    Variable = "Observations",
    Total_Profit = nobs(mod1),
    Log_Employees = nobs(mod2),
    Firm_Borrowing = nobs(mod3),
    Shutdown = nobs(mod4)
  )
)

# Appending R-squared values
table2 <- rbind(
  table2,
  data.frame(
    Variable = "R-squared",
    Total_Profit = formatC(summary(mod1)$r.squared, format = "f", digits = 4),
    Log_Employees = formatC(summary(mod2)$r.squared, format = "f", digits = 4),
    Firm_Borrowing = formatC(summary(mod3)$r.squared, format = "f", digits = 4),
    Shutdown = formatC(summary(mod4)$r.squared, format = "f", digits = 4)
  )
)

# Table 2
cat("\nTable 2: Regression Results (Estimates Only)\n")
print(table2)

                              ## Question 6

# round2_data
round2_data <- loan %>%
  filter(round == 2) %>%
  filter(!is.na(type) & !is.na(retail) & !is.na(age) & !is.na(gender) &
           !is.na(education_college) & !is.na(total_profit) &
           !is.na(lnlabor) & !is.na(allloan_end) & !is.na(shutdownall))

# Fitting original models from 5B
model1 <- lm(total_profit ~ type + retail + age + gender + education_college, data = round2_data)
model2 <- lm(lnlabor ~ type + retail + age + gender + education_college, data = round2_data)
model3 <- lm(allloan_end ~ type + retail + age + gender + education_college, data = round2_data)
model4 <- lm(shutdownall ~ type + retail + age + gender + education_college, data = round2_data)

# Getting residuals e1 
e1_total_profit <- residuals(lm(total_profit   ~ retail + age + gender + education_college, data = round2_data))
e1_lnlabor <- residuals(lm(lnlabor        ~ retail + age + gender + education_college, data = round2_data))
e1_allloan_end <- residuals(lm(allloan_end    ~ retail + age + gender + education_college, data = round2_data))
e1_shutdownall <- residuals(lm(shutdownall    ~ retail + age + gender + education_college, data = round2_data))

# Getting residuals e2 
e2_type <- residuals(lm(I(type - mean(type)) ~ retail + age + gender + education_college, data = round2_data))

# Regressing e1 on e2
resmod1 <- lm(e1_total_profit ~ e2_type)
resmod2 <- lm(e1_lnlabor ~ e2_type)
resmod3 <- lm(e1_allloan_end ~ e2_type)
resmod4 <- lm(e1_shutdownall ~ e2_type)

# Comparing coefficients
orig_coefs <- c(coef(model1)["type"],
                coef(model2)["type"],
                coef(model3)["type"],
                coef(model4)["type"])

resid_coefs <- c(coef(resmod1)["e2_type"],
                 coef(resmod2)["e2_type"],
                 coef(resmod3)["e2_type"],
                 coef(resmod4)["e2_type"])

comparison <- data.frame(
  Outcome = c("Total Profit", "Log Employees", "Firm Borrowing", "Shutdown"),
  Original_Type_Coefficient = round(orig_coefs, 6),
  Residual_Regression_Coefficient = round(resid_coefs, 6),
  Identical_UpTo6Digits = round(orig_coefs, 6) == round(resid_coefs, 6)
)

print(comparison)

stopifnot(all.equal(unname(round(orig_coefs, 6)), unname(round(resid_coefs, 6))))

                              ## Question 7

# Computing R^2 using formula
manual_r2 <- function(model, data, y_var) {
  y_actual <- data[[y_var]]
  y_hat <- predict(model, data)
  residuals <- y_actual - y_hat
  ss_res <- sum(residuals^2)
  ss_tot <- sum((y_actual - mean(y_actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  return(r_squared)
}

# Computing R^2 for all four models from Question
r2_1_manual <- manual_r2(model1, round2_data, "total_profit")
r2_2_manual <- manual_r2(model2, round2_data, "lnlabor")
r2_3_manual <- manual_r2(model3, round2_data, "allloan_end")
r2_4_manual <- manual_r2(model4, round2_data, "shutdownall")

# Extracting R^2 from lm() summary
r2_1_lm <- summary(model1)$r.squared
r2_2_lm <- summary(model2)$r.squared
r2_3_lm <- summary(model3)$r.squared
r2_4_lm <- summary(model4)$r.squared

# Logical assertion
stopifnot(all.equal(r2_1_manual, r2_1_lm, tolerance = 1e-6))
stopifnot(all.equal(r2_2_manual, r2_2_lm, tolerance = 1e-6))
stopifnot(all.equal(r2_3_manual, r2_3_lm, tolerance = 1e-6))
stopifnot(all.equal(r2_4_manual, r2_4_lm, tolerance = 1e-6))

# Printing the values side by side
cat("R^2 Comparison (Manual vs lm()):\n")
cat("1. Total Profit     :", round(r2_1_manual, 6), "==", round(r2_1_lm, 6), "\n")
cat("2. Log Employees    :", round(r2_2_manual, 6), "==", round(r2_2_lm, 6), "\n")
cat("3. Firm Borrowing   :", round(r2_3_manual, 6), "==", round(r2_3_lm, 6), "\n")
cat("4. Shutdown Outcome :", round(r2_4_manual, 6), "==", round(r2_4_lm, 6), "\n")


                               ## Question 8

# Add'beluga'
set.seed(582)
round2_data$beluga <- runif(nrow(round2_data)) 

# Models Without beluga
m1 <- lm(total_profit ~ type + retail + age + gender + education_college, data = round2_data)
m2 <- lm(lnlabor ~ type + retail + age + gender + education_college, data = round2_data)
m3 <- lm(allloan_end ~ type + retail + age + gender + education_college, data = round2_data)
m4 <- lm(shutdownall ~ type + retail + age + gender + education_college, data = round2_data)

# Models With beluga
m1_b <- lm(total_profit ~ type + retail + age + gender + education_college + beluga, data = round2_data)
m2_b <- lm(lnlabor ~ type + retail + age + gender + education_college + beluga, data = round2_data)
m3_b <- lm(allloan_end ~ type + retail + age + gender + education_college + beluga, data = round2_data)
m4_b <- lm(shutdownall ~ type + retail + age + gender + education_college + beluga, data = round2_data)

# Coefficient tables
tab_without <- data.frame(
  Variable = rownames(coef(summary(m1))),
  Total_Profit = coef(m1),
  Log_Employees = coef(m2),
  Firm_Borrowing = coef(m3),
  Shutdown = coef(m4)
)

tab_with <- data.frame(
  Variable = rownames(coef(summary(m1_b))),
  Total_Profit_Beluga = coef(m1_b),
  Log_Employees_Beluga = coef(m2_b),
  Firm_Borrowing_Beluga = coef(m3_b),
  Shutdown_Beluga = coef(m4_b)
)

# Merging both tables
table3 <- merge(tab_without, tab_with, by = "Variable", all = TRUE)

# Adding observations, R², Adjusted R² at the bottom
table3 <- rbind(
  table3,
  data.frame(
    Variable = "Observations",
    Total_Profit = nobs(m1), Log_Employees = nobs(m2),
    Firm_Borrowing = nobs(m3), Shutdown = nobs(m4),
    Total_Profit_Beluga = nobs(m1_b), Log_Employees_Beluga = nobs(m2_b),
    Firm_Borrowing_Beluga = nobs(m3_b), Shutdown_Beluga = nobs(m4_b)
  ),
  data.frame(
    Variable = "R-squared",
    Total_Profit = summary(m1)$r.squared, Log_Employees = summary(m2)$r.squared,
    Firm_Borrowing = summary(m3)$r.squared, Shutdown = summary(m4)$r.squared,
    Total_Profit_Beluga = summary(m1_b)$r.squared, Log_Employees_Beluga = summary(m2_b)$r.squared,
    Firm_Borrowing_Beluga = summary(m3_b)$r.squared, Shutdown_Beluga = summary(m4_b)$r.squared
  ),
  data.frame(
    Variable = "Adjusted R-squared",
    Total_Profit = summary(m1)$adj.r.squared, Log_Employees = summary(m2)$adj.r.squared,
    Firm_Borrowing = summary(m3)$adj.r.squared, Shutdown = summary(m4)$adj.r.squared,
    Total_Profit_Beluga = summary(m1_b)$adj.r.squared, Log_Employees_Beluga = summary(m2_b)$adj.r.squared,
    Firm_Borrowing_Beluga = summary(m3_b)$adj.r.squared, Shutdown_Beluga = summary(m4_b)$adj.r.squared
  )
)

# Printing Table 3
cat("\nTable 3: Regression Results With and Without Beluga\n")
print(table3, digits = 6)

                               ## Question 9

# Fitting the regression model without beluga
model <- lm(total_profit ~ type + retail + age + gender + education_college, data = round2_data)

# Computing leverage values (hii)
hii <- hatvalues(model)

# Confirming hii are non-negative and sum equals k
all(hii >= 0)                
sum_hii <- sum(hii)          
k <- length(coef(model))     
all.equal(sum_hii, k)

# Computing least squares residuals (ei)
residuals <- resid(model)

# Computing prediction errors (ε̃i)
pred_errors <- residuals / (1 - hii)

# Computing difference: ei - ε̃i
diff_residuals <- residuals - pred_errors

# Plot: ei - ε̃i vs hii
plot(hii, diff_residuals,
     xlab = "Leverage values (hii)",
     ylab = "Difference (ei - ε̃i)",
     main = "Difference Between Residuals and Prediction Errors vs. Leverage",
     pch = 20, col = "darkblue")
abline(h = 0, col = "red", lty = 2)

png("diff_vs_hii.png", width = 800, height = 600)
plot(hii, diff_residuals,
     xlab = "Leverage values (hii)",
     ylab = "Difference (ei - ε̃i)",
     main = "Difference Between Residuals and Prediction Errors vs. Leverage",
     pch = 20, col = "darkblue")
abline(h = 0, lty = 2, col = "red")
dev.off()

                              ## Question 10a

set.seed(582)
orca <- rnorm(nrow(round2_data), mean = 0, sd = 7)

# Creating the erroneous dependent variable
round2_data$err_total_profit <- round2_data$total_profit + orca

# Re-estimating the model using err_total_profit
model_err_total_profit <- lm(err_total_profit ~ type + retail + age + gender + education_college, data = round2_data %>% filter(round == 2))

# Summary of the regression
summary(model_err_total_profit)

                               ## Question 10b

set.seed(582)
narwhal <- rnorm(nrow(round2_data), mean = 0, sd = 2)

# Creating the erroneous independent variable
round2_data$err_age <- round2_data$age + narwhal

# Re-estimating the model using err_age
model_err_age <- lm(total_profit ~ type + retail + err_age + gender + education_college, data = round2_data %>% filter(round == 2))

# Summary of the regression
summary(model_err_age)

                              ## Question 10c

original_model <- lm(total_profit ~ type + retail + age + gender + education_college, data = round2_data %>% filter(round == 2))

original_results <- tidy(original_model) %>%
  select(term, estimate) %>%
  rename(Original = estimate)

# Extracting results
err_total_profit_results <- tidy(model_err_total_profit) %>%
  select(term, estimate) %>%
  rename(Err_Total_Profit = estimate)

err_age_results <- tidy(model_err_age) %>%
  select(term, estimate) %>%
  rename(Err_Age = estimate)

# Combining all results into one table
table4 <- original_results %>%
  left_join(err_total_profit_results, by = "term") %>%
  left_join(err_age_results, by = "term")

# Table 4
print(table4)

                                ## Question 11

# Selecting all variables used in the regressions
cols <- c("type", "retail", "age", "gender", "education_college", 
          "total_profit", "lnlabor", "allloan_end", "shutdownall")

# Creating demeaned data
demeaned_data <- round2_data %>%
  select(all_of(cols)) %>%
  mutate(across(everything(), ~ .x - mean(.x))) 

# Running regressions on demeaned data
mod1_dm <- lm(total_profit ~ type + retail + age + gender + education_college - 1, data = demeaned_data)
mod2_dm <- lm(lnlabor ~ type + retail + age + gender + education_college - 1, data = demeaned_data)
mod3_dm <- lm(allloan_end ~ type + retail + age + gender + education_college - 1, data = demeaned_data)
mod4_dm <- lm(shutdownall ~ type + retail + age + gender + education_college - 1, data = demeaned_data)

# Comparing coefficients with original models (from 5B) using assertion
compare_demeaned <- function(orig_model, demeaned_model) {
  all.equal(round(coef(orig_model)[-1], 6), round(coef(demeaned_model), 6))
}

cat("Comparison of demeaned regressions to original (Table 2):\n")
cat("1. Total Profit     :", compare_demeaned(model1, mod1_dm), "\n")
cat("2. Log Employees    :", compare_demeaned(model2, mod2_dm), "\n")
cat("3. Firm Borrowing   :", compare_demeaned(model3, mod3_dm), "\n")
cat("4. Shutdown         :", compare_demeaned(model4, mod4_dm), "\n")

# Building Table 5 comparing original and demeaned coefficients
get_named_coefs <- function(model) coef(model)
t1 <- get_named_coefs(model1)[-1] 
t2 <- get_named_coefs(mod1_dm)
l1 <- get_named_coefs(model2)[-1]
l2 <- get_named_coefs(mod2_dm)
f1 <- get_named_coefs(model3)[-1]
f2 <- get_named_coefs(mod3_dm)
s1 <- get_named_coefs(model4)[-1]
s2 <- get_named_coefs(mod4_dm)

# Variables
all_vars <- names(t2)

# Coefficient comparison table
table5 <- data.frame(
  Variable = all_vars,
  Total_Profit_Orig = round(t1, 6),
  Total_Profit_Demeaned = round(t2, 6),
  Log_Employees_Orig = round(l1, 6),
  Log_Employees_Demeaned = round(l2, 6),
  Firm_Borrowing_Orig = round(f1, 6),
  Firm_Borrowing_Demeaned = round(f2, 6),
  Shutdown_Orig = round(s1, 6),
  Shutdown_Demeaned = round(s2, 6)
)

# Adding rows for observations and R-squared
obs_row <- data.frame(
  Variable = "Observations",
  Total_Profit_Orig = nobs(model1), Total_Profit_Demeaned = nobs(mod1_dm),
  Log_Employees_Orig = nobs(model2), Log_Employees_Demeaned = nobs(mod2_dm),
  Firm_Borrowing_Orig = nobs(model3), Firm_Borrowing_Demeaned = nobs(mod3_dm),
  Shutdown_Orig = nobs(model4), Shutdown_Demeaned = nobs(mod4_dm)
)

r2_row <- data.frame(
  Variable = "R-squared",
  Total_Profit_Orig = round(summary(model1)$r.squared, 4), Total_Profit_Demeaned = round(summary(mod1_dm)$r.squared, 4),
  Log_Employees_Orig = round(summary(model2)$r.squared, 4), Log_Employees_Demeaned = round(summary(mod2_dm)$r.squared, 4),
  Firm_Borrowing_Orig = round(summary(model3)$r.squared, 4), Firm_Borrowing_Demeaned = round(summary(mod3_dm)$r.squared, 4),
  Shutdown_Orig = round(summary(model4)$r.squared, 4), Shutdown_Demeaned = round(summary(mod4_dm)$r.squared, 4)
)

# Combining into final Table 5
table5 <- bind_rows(table5, obs_row, r2_row)

# Table 5
cat("\nTable 5: Comparison of Original and Demeaned Regression Results\n")
print(table5)

sink()
close(logf)