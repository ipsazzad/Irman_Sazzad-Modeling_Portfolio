library(tidyverse)

setwd("E:/Study/Fall 25/Metrix/PS/2/")
sink("log.txt")

#Question 6

#a.
y <- c(142, 18, 41, 133, 7)
X <- matrix(c(
  20, 8, 1,
  12, 3, 1,
  10, 4, 1,
  10, 8, 1,
  8, 1, 1
), nrow = 5, byrow = TRUE)

b_hat <- solve(t(X) %*% X) %*% t(X) %*% y

reg <- lm(y ~ X)
summary(reg)

#b.
rows <- nrow(X)
I <- diag(rows)

# projection and annihilator
P <- X %*% solve(t(X) %*% X) %*% t(X)
M <- diag(nrow(X)) - P

# check idempotent
stopifnot(all.equal(M %*% M, M))

# check that M * y gives residuals
resids_matrix <- M %*% y
resids_lm <- residuals(reg)

stopifnot(all.equal(as.vector(resids_matrix), unname(resids_lm)))

#c.
# Extract x1 and regress it on x2 and intercept
x1 <- X[,1] 
x2 <- X[,2] 
int <- rep(1, length(x1)) 

# Combine x2 and intercept into one matrix
X_other <- cbind(x2, int)

# Regress x1 on x2 and intercept
x1_on_x2 <- lm(x1 ~ X_other)
U <- residuals(x1_on_x2) 

# Use decomposition formula:
beta1_decomp <- sum(U * y) / sum(U^2)

# Compare with matrix-derived beta1
stopifnot(all.equal(beta1_decomp, b_hat[1,1]))

#d.
b1s <- c(4.8, 5.2, 5.5, 5.812, 6.0, 6.2)
sses <- c(500, 300, 200, 180, 210, 280)
print(b_hat[1,1])
# Create the plot
plot(b1s, sses,
     xlab = "Beta 1",
     ylab = "SSE")
abline(v = 5.812, col = "green")

#Question 7

#a.
Data <- read.csv("CollegeScorecardInstitution_2024.csv", header = TRUE, sep = ",")

clg <- Data %>%
  filter(CONTROL == 1,
         ICLEVEL == 1,
         MAIN == 1,
         HIGHDEG >= 3)

#b.
clean_clg <- clg %>%
  select(UNITID, MD_EARN_WNE_P10, SAT_AVG, DEBT_MDN, ADM_RATE) %>%
  drop_na()

#c.
clean_clg <- clean_clg %>%
  mutate(sat_grp = cut(SAT_AVG,
                       breaks = quantile(SAT_AVG, probs = seq(0, 1, 0.25), na.rm = TRUE),
                       labels = c("SAT_Q1", "SAT_Q2", "SAT_Q3", "SAT_Q4"),
                       include.lowest = TRUE),
         adm_grp = cut(ADM_RATE,
                       breaks = quantile(ADM_RATE, probs = seq(0, 1, 0.25), na.rm = TRUE),
                       labels = c("ADM_Q1", "ADM_Q2", "ADM_Q3", "ADM_Q4"),
                       include.lowest = TRUE))
# Computing (CEF)
cef_tbl <- clean_clg %>%
  group_by(sat_grp, adm_grp) %>%
  summarize(pred_cef = mean(MD_EARN_WNE_P10, na.rm = TRUE)) %>%
  ungroup()
# Merging back into original data
clean_clg <- clean_clg %>%
  left_join(cef_tbl, by = c("sat_grp", "adm_grp"))
# Calculating mean residual and mean squared prediction error
mean_resid_cef <- mean(clean_clg$MD_EARN_WNE_P10 - clean_clg$pred_cef)
mse_cef <- mean((clean_clg$MD_EARN_WNE_P10 - clean_clg$pred_cef)^2)

#d.
# Converting DEBT_MDN to numeric
clean_clg <- clean_clg %>%
  mutate(DEBT_MDN = as.numeric(gsub("[$,]", "", DEBT_MDN))) %>%
  drop_na(DEBT_MDN)  # drop rows where conversion produced NA
clean_clg <- clean_clg %>%
  mutate(debt_grp = cut(DEBT_MDN,
                        breaks = quantile(DEBT_MDN, probs = seq(0, 1, 0.25), na.rm = TRUE),
                        labels = c("DEBT_Q1", "DEBT_Q2", "DEBT_Q3", "DEBT_Q4"),
                        include.lowest = TRUE))
# Computing CEF
cef_tbl_d <- clean_clg %>%
  group_by(sat_grp, adm_grp, debt_grp) %>%
  summarize(pred_cef_d = mean(MD_EARN_WNE_P10, na.rm = TRUE), .groups = "drop")
# Merging predictions back
clean_clg <- clean_clg %>%
  left_join(cef_tbl_d, by = c("sat_grp", "adm_grp", "debt_grp"))
# Calculating mean residual and mean squared prediction error
mean_resid_cef_d <- mean(clean_clg$MD_EARN_WNE_P10 - clean_clg$pred_cef_d)
mse_cef_d <- mean((clean_clg$MD_EARN_WNE_P10 - clean_clg$pred_cef_d)^2)

#f.
lm_model_d <- lm(MD_EARN_WNE_P10 ~ SAT_AVG + ADM_RATE + DEBT_MDN, data = clean_clg)
clean_clg$pred_lm_d <- predict(lm_model_d, clean_clg)
# Compute mean residual and mean squared prediction error
mean_resid_lm_d <- mean(clean_clg$MD_EARN_WNE_P10 - clean_clg$pred_lm_d)
mse_lm_d <- mean((clean_clg$MD_EARN_WNE_P10 - clean_clg$pred_lm_d)^2)

sink()