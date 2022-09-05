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

hotelling_test_data <- data.frame(X1, X2, X3)


data_moments <- function(data){
  meanvec <- matrix(apply(data, 2, mean), ncol = 1)
  covmat <- matrix(cov(data), ncol = dim(data)[2])
  return(list(meanvec,covmat))
  #should we use super-assignment instead of return?
}


xbar1 <- data_moments(hotelling_test_data)[1]
covmat1 <- data_moments(hotelling_test_data)[2]
xbar1 # returns a list, we need a px1 matrix
covmat1 # returns a list, we need a pxp matrix
class(xbar1)
str(xbar1)
class(covmat1)
str(covmat1)





# mean matrix under null hypothesis
mu_null <- matrix(c(4, 50, 10), nrow = dim(hotelling_test_data)[2])



#' Function for Hotelling T2 test using specified data matrix and null mean matrix
#' 
#' @param data A dataframe (nxp) with n observations of p variables
#' @param mu_null A px1 matrix for the mean vector under the null hypothesis
#' @param alpha The level of significance for the test (default value is 0.05)
#' @return The T2 statistic, the critical value and the result of the test.
#' @examples 
#' hotelling_T2_test(hotelling_test_data, mu_null, 0.1)
#' hotelling_T2_test(hotelling_test_data, mu_null)
hotelling_T2_test <- function(data, mu_null, alpha = 0.05){
  n <- dim(data)[1]
  p <- dim(data)[2]
  S <- cov(data)
  xbar <- matrix(colMeans(data), 
                 nrow = p) #creates px1 matrix of observed means
  T2 <- n * t(xbar - mu_null) %*% solve(S) %*% (xbar - mu_null)
  crit_val <- (n-1)*p/(n-p) * qf((1-alpha), p, n-p)
  writeLines(c("This command runs the Hotelling T2 Hypothesis Test.",
               "The command's arguments specify", 
               "(1) the data,",
               "(2) the value of the mean vector under the null hypothesis, and",
               "(3) the level of significance for the test (i.e., alpha).",
               "",
               "Results of the test:",
               "",
               paste0("1. The Hotelling T2 statistic is: ", round(T2, 4)),
               paste0("2. The critical value for the test is: ", round(crit_val, 4)),
               paste0("3. The test was run at level of significance alpha = ", alpha)))
  if(T2 > crit_val){
    writeLines(c("4. The Hotelling T2 statistic exceeds the critical value, therefore",
                 paste0("at level of significance alpha = ",alpha," there is enough evidence"),
                 "to conclude that the true mean vector differs from the value specified",
                 "in the null hypothesis."))
  }
  else{
    writeLines(c("4. The Hotelling T2 statistic does not exceed the critical value, therefore",
                 paste0("at level of significance alpha = ",alpha," there is NOT enough evidence"),
                 "to conclude that the true mean vector differs from the value specified",
                 "in the null hypothesis."))
  }
}

# Running the test

hotelling_T2_test(hotelling_test_data, mu_null, 0.1)
hotelling_T2_test(hotelling_test_data, mu_null)



