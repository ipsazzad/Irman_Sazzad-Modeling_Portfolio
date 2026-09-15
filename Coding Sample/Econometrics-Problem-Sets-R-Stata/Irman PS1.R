#a. Working directory
setwd("E:/Study/Fall 25/Metrix/PS/1/Q6")

#b. Log.
sink("the_log.txt")  # Sink Opening
cat("Logging begins...\n\n")

#c. Matrices
A <- matrix(c(2, 0, 1,
              4, 3, 2,
              0, 9, 1), 
            nrow = 3, byrow = TRUE)

B <- matrix(c(0, 0, 1,
              1, 1, 0), 
            nrow = 2, byrow = TRUE)

C <- matrix(c(4, 9, 0,
              7, 4, 8), 
            nrow = 2, byrow = TRUE)

cat("Matrix A:\n"); print(A)
cat("\nMatrix B:\n"); print(B)
cat("\nMatrix C:\n"); print(C)

#d. AB'
ABt <- A %*% t(B)
cat("\nMatrix AB':\n")
print(ABt)

sink()  # Sink Closing

#e. Non-conforming Matrix product
cat("\n Trying A %*% B:\n")
A %*% B  # The error indicates the matrices are not conformable.

#f. (AB')' = BA'?
stopifnot(all.equal(t(ABt), B %*% t(A)))
"Success: (AB')' equals BA'" # So, the equality holds as there's no error.

#g. Cyclic permutation
traced <- function(M) {
  return(sum(diag(M)))
}

ABpC <- A %*% t(B) %*% C
print(ABpC)
print(traced(ABpC)) #compute trace

BpCA <- t(B) %*% C %*% A
print(BpCA)
print(traced(BpCA)) #compute trace

stopifnot(all.equal(traced(ABpC), traced(BpCA))) # Assertion
print("Success: trace(AB'C) equals trace(B'CA)") #So, cyclic permutations under trace preserve the result.

#h. Inverse
Ainv <- solve(A)

identity_check <- A %*% Ainv
print(identity_check) # Formed the identity matrix

#i. load, mean, SD
loan <- read.csv("loanmain.csv")
mean_lnrev <- mean(loan$lnpart5revenue, na.rm = TRUE)
sd_lnrev <- sd(loan$lnpart5revenue, na.rm = TRUE)
mean_profit <- mean(loan$total_profit, na.rm = TRUE)
sd_profit <- sd(loan$total_profit, na.rm = TRUE)

# Remove NA
x <- loan$lnpart5revenue
x <- x[!is.na(x)]
y <- loan$total_profit
y <- y[!is.na(y)]

# lnpart5revenue
n1 <- length(x)
manual_mean_x <- sum(x) / n1
manual_sd_x <- sqrt(sum((x - manual_mean_x)^2) / (n1 - 1))

command_mean_x <- mean(x)
command_sd_x <- sd(x)

stopifnot(all.equal(manual_mean_x, command_mean_x))
stopifnot(all.equal(manual_sd_x, command_sd_x))

print(manual_mean_x)
print(command_mean_x)
print(manual_sd_x)
print(command_sd_x)

#total_profit
n2 <- length(y)
manual_mean_y <- sum(y) / n2
manual_sd_y <- sqrt(sum((y - manual_mean_y)^2) / (n2 - 1))

command_mean_y <- mean(y)
command_sd_y <- sd(y)

stopifnot(all.equal(manual_mean_y, command_mean_y))
stopifnot(all.equal(manual_sd_y, command_sd_y))

print(manual_mean_y)
print(command_mean_y)
print(manual_sd_y)
print(command_sd_y)

#j. Group-wise mean by type
mean_newloan_by_type <- sapply(split(loan$newloan, loan$type), mean, na.rm = TRUE)
print(mean_newloan_by_type)
