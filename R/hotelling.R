# Example data


X1 <- c(3.7, 5.7, 3.8, 3.2, 3.1,
        4.6, 2.4, 7.2, 6.7, 5.4,
        3.9, 4.5, 3.5, 4.5, 1.5,
        8.5, 4.5, 6.5, 4.1, 5.5)
X2 <- c(48.5, 65.1, 47.2, 53.2, 55.5,
        36.1, 24.8, 33.1, 47.4, 54.1,
        36.9, 58.8, 27.8, 40.2, 13.5,
        56.4, 71.6, 52.8, 44.1, 40.9)
X3 <- c(9.3, 8, 10.9, 12, 9.7,
        7.9, 14, 7.6, 8.5, 11.3,
        12.7, 12.3, 9.8, 8.4, 10.1,
        7.1, 8.2, 10.9, 11.2, 9.4)

data <- data.frame(X1, X2, X3)

# Hotelling hypothesis test
# Need data matrix nxp and null mean matrix px1
n <- dim(data)[1]
p <- dim(data)[2]
# null mean matrix
mu_null <- matrix(c(4, 50, 10), nrow = dim(data)[2])
# function for Hotelling T2, the test statistic
hotel_T2 <- function(data, mu_null){
  n <- dim(data)[1]
  p <- dim(data)[2]
  S <- cov(data)
  xbar <- matrix(colMeans(data), 
                 nrow = p) #creates px1 matrix of observed means
  T2 <- n * t(xbar - mu_null) %*% solve(S) %*% (xbar - mu_null)
}
# calculation of Hotelling T2
T2 <- hotel_T2(data, mu_null)
T2

# calculation of critical value (F statistic)
alph <- 0.1
crit_val <- (n-1)*p/(n-p) * qf(.9, p, n-p)
crit_val



